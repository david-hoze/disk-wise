{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.CLI
  ( runApp
  , runInvestigate
  , deduplicateContribs
  ) where

import Control.Exception (catch, SomeException)
import Data.IORef
import Data.List (isPrefixOf, partition)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory,
                         listDirectory, doesDirectoryExist, removeFile)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))

import DiskWise.Types
import DiskWise.WikiRouter
import DiskWise.Claude (investigate, callClaude, buildSystemPrompt, buildLearnPrompt,
                        parseAdvice, agentIdentity, prefixCommitMsg, formatCommandStats)
import DiskWise.Scanner
import DiskWise.History (saveSessionSummary, loadSessionHistory, loadMostRecentSummary,
                         summarizeSession, formatSessionHistory, computeCommandStats,
                         detectDiminishingReturns)

-- | Main application entry point
runApp :: AppConfig -> IO ()
runApp config = do
  hSetBuffering stdin LineBuffering

  TIO.putStrLn "+===========================================+"
  TIO.putStrLn "|           DiskWise v2.0.0                 |"
  TIO.putStrLn "|   AI-powered disk cleanup with shared     |"
  TIO.putStrLn "|   wiki knowledge                          |"
  TIO.putStrLn "+===========================================+"
  TIO.putStrLn ""

  -- Retry any pending contributions from previous sessions
  retryPending config

  mainLoop config

mainLoop :: AppConfig -> IO ()
mainLoop config = do
  TIO.putStrLn "What would you like to do?"
  TIO.putStrLn "  [i] Investigate (scan + analyze + learn)"
  TIO.putStrLn "  [q] Quit"
  TIO.putStr "> "
  hFlush stdout

  choice <- getLine
  case choice of
    "i" -> do
      runInvestigate config
      mainLoop config
    "q" -> TIO.putStrLn "Goodbye!"
    "\\q" -> TIO.putStrLn "Goodbye!"
    _   -> do
      TIO.putStrLn "Unknown option, try again."
      mainLoop config

-- | Full investigation flow: scan -> parse -> wiki -> match -> Claude -> act -> learn -> refactor
runInvestigate :: AppConfig -> IO ()
runInvestigate config = do
  platform <- detectPlatform
  sessionRef <- newIORef emptySessionLog { logPlatform = platform }
  identity <- agentIdentity
  history <- loadSessionHistory

  -- Step 0: Follow up on previous session
  prevSummary <- loadMostRecentSummary
  regrowthReport <- checkRegrowth prevSummary
  askDelayedFeedback sessionRef prevSummary

  -- Step 1: Scan the system
  TIO.putStrLn "\n-- Scanning system --\n"
  scanOutput <- scanSystem config
  modifyIORef sessionRef (\s -> s { logScanOutput = scanOutput })

  -- Step 2: Parse findings
  let minBytes = configMinSizeMB config * 1024 * 1024
      findings = parseFindings minBytes scanOutput
  modifyIORef sessionRef (\s -> s { logFindings = findings })
  TIO.putStrLn $ "Found " <> T.pack (show (length findings)) <> " items of interest.\n"

  -- Step 3: Fetch wiki pages (graceful failure -> empty list)
  TIO.putStrLn "-- Fetching wiki knowledge --\n"
  wikiPages <- fetchWikiGracefully config

  -- Step 4: Separate observation pages (hard constraints) from tool pages
  let isObservationPage p = "observations/" `isPrefixOf` pageRelPath p
      (observationPages, toolPages) = partition isObservationPage wikiPages

  -- Step 4b: Match tool pages to findings (Claude-assisted, with heuristic fallback)
  matched <- matchPagesWithClaude (callClaude config) toolPages findings
  let matchedFindingPaths = concatMap (map findingPath . snd) matched
      novelFindings = filter (\f -> findingPath f `notElem` matchedFindingPaths) findings

  TIO.putStrLn $ "Wiki matched " <> T.pack (show (length matched))
              <> " page(s) to findings."
  TIO.putStrLn $ T.pack (show (length novelFindings))
              <> " finding(s) not covered by wiki.\n"

  -- Step 5: Call Claude for investigation
  TIO.putStrLn "-- Asking Claude to analyze --\n"
  let cmdStats = computeCommandStats history
      prevCleaned = maybe [] summaryCleanedPaths prevSummary
      diminishing = detectDiminishingReturns history
  result <- investigate config scanOutput matched novelFindings cmdStats prevCleaned observationPages wikiPages diminishing
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> T.pack (show err)
    Right advice -> do
      modifyIORef sessionRef (\s -> s { logAdvice = Just advice })

      -- Step 6: Present analysis
      presentAdvice advice

      -- Step 7: Offer cleanup actions (tracking results in session)
      offerCleanup config sessionRef wikiPages (adviceCleanupActions advice)

      -- Step 8: Detect steady state
      do session' <- readIORef sessionRef
         let executed = [() | ActionExecuted _ <- logEvents session']
             proposed = adviceCleanupActions advice
             steadyState = case diminishing of
               Just _  -> null proposed || null executed
               Nothing -> False
         when steadyState $ do
           TIO.putStrLn "-- Steady state --\n"
           TIO.putStrLn "Safe automated cleanup has been exhausted on this system."
           TIO.putStrLn "Remaining space recovery requires decisions only you can make:"
           TIO.putStrLn "  - Remove build artifacts from specific projects"
           TIO.putStrLn "  - Uninstall tools or SDKs you no longer use"
           TIO.putStrLn "  - Move large files to external storage"
           TIO.putStrLn ""
           TIO.putStrLn "Run diskwise again after making changes or installing new software.\n"

      -- Step 9: Post-cleanup feedback (before learning so feedback is visible)
      postCleanupFeedback sessionRef

      -- Step 10: Session-aware learning
      -- Skip learn phase when diminishing returns are active — the wiki
      -- is already well-populated and further contributions add noise.
      case diminishing of
        Just _ -> do
          TIO.putStrLn "-- Skipping learning phase (diminishing returns active) --\n"
          -- Still offer investigate-phase contributions (usually 0-1)
          _ <- offerLearn config sessionRef wikiPages (adviceContributions advice)
          pure ()
        Nothing -> do
          TIO.putStrLn "-- Learning from session --\n"
          session' <- readIORef sessionRef
          let cmdStatsText = formatCommandStats cmdStats
              historyContext = formatSessionHistory history
                           <> (if T.null regrowthReport then "" else "\n" <> regrowthReport)
                           <> (if T.null cmdStatsText then "" else "\n" <> cmdStatsText)
          learnResult <- callClaude config buildSystemPrompt
            (buildLearnPrompt session' identity historyContext wikiPages diminishing)
          let allContribs = case learnResult of
                Right text -> case parseAdvice text of
                  Right learnAdvice -> adviceContributions advice <> adviceContributions learnAdvice
                  Left _ -> adviceContributions advice
                Left _ -> adviceContributions advice
          _ <- offerLearn config sessionRef wikiPages allContribs
          pure ()

      -- Step 11: Save session summary for cross-session learning
      finalSession <- readIORef sessionRef
      summary <- summarizeSession finalSession
      saveSessionSummary summary

-- | Fetch wiki pages, returning empty list on any failure
fetchWikiGracefully :: AppConfig -> IO [WikiPage]
fetchWikiGracefully config = do
  result <- fetchTree config
    `catch` (\(_ :: SomeException) -> pure (Left WikiNotAvailable))
  case result of
    Right pages -> do
      TIO.putStrLn $ "Loaded " <> T.pack (show (length pages)) <> " wiki page(s)."
      pure pages
    Left err -> do
      TIO.putStrLn $ "Wiki unavailable (" <> T.pack (show err) <> "), proceeding without."
      pure []

-- | Display Claude's analysis
presentAdvice :: ClaudeAdvice -> IO ()
presentAdvice advice = do
  TIO.putStrLn "Claude's Analysis:"
  TIO.putStrLn "-------------------"
  TIO.putStrLn (adviceAnalysis advice)
  TIO.putStrLn ""

-- | Offer cleanup actions one by one, tracking results in the session log
offerCleanup :: AppConfig -> IORef SessionLog -> [WikiPage] -> [CleanupAction] -> IO ()
offerCleanup _ _ _ [] = TIO.putStrLn "No cleanup actions suggested.\n"
offerCleanup config sessionRef pages actions = do
  TIO.putStrLn $ "-- " <> T.pack (show (length actions)) <> " cleanup action(s) suggested --\n"
  execCountRef <- newIORef (0 :: Int)
  mapM_ (\(pos, action) -> offerOne config sessionRef execCountRef pages pos action)
        (zip [0..] actions)
  TIO.putStrLn ""
  where
    offerOne cfg ref execRef pgs position action = do
      TIO.putStrLn $ "  Action:   " <> actionDescription action
      TIO.putStrLn $ "  Command:  " <> actionCommand action
      TIO.putStrLn $ "  Risk:     " <> actionRiskLevel action
      case actionSizeEstimate action of
        Just est -> TIO.putStrLn $ "  Estimate: " <> est
        Nothing  -> pure ()
      case actionWikiRef action of
        Just wref -> TIO.putStrLn $ "  Wiki ref: " <> wref
        Nothing  -> pure ()

      -- Dry-run validation
      vr <- validateAction action
      mapM_ (\w -> TIO.putStrLn $ "  WARNING: " <> w) (validationWarnings vr)

      let promptExecute = do
            TIO.putStr "  Execute? [y/n] > "
            hFlush stdout
            answer <- getLine
            case answer of
              "y" -> pure True
              "n" -> pure False
              _   -> do
                TIO.putStrLn "  Please enter y or n."
                promptExecute
      execute <- promptExecute
      if execute
        then do
          execOrder <- readIORef execRef
          modifyIORef execRef (+ 1)
          freeBefore <- measureDiskFree
          result <- runCleanupAction action
          freeAfter <- measureDiskFree
          let bytesFreed = do
                before <- freeBefore
                after <- freeAfter
                let diff = after - before
                pure (if diff > 0 then diff else 0)
          case result of
            Right msg -> do
              TIO.putStrLn $ "  OK: " <> msg
              case bytesFreed of
                Just b | b > 0 -> TIO.putStrLn $ "  Freed: ~"
                  <> T.pack (show (b `div` (1024 * 1024))) <> " MB"
                _ -> pure ()
              let outcome = CleanupOutcome
                    { outcomeAction = action
                    , outcomeSuccess = True
                    , outcomeMessage = msg
                    , outcomeBytesFreed = bytesFreed
                    , outcomeExpected = actionSizeEstimate action
                    , outcomePosition = position
                    , outcomeOrder = execOrder
                    }
              modifyIORef ref (`addEvent` ActionExecuted outcome)
              case actionWikiRef action of
                Just wref -> recordOutcome cfg pgs (T.unpack wref) True
                Nothing   -> pure ()
            Left err -> do
              TIO.putStrLn $ "  Error: " <> err
              let outcome = CleanupOutcome
                    { outcomeAction = action
                    , outcomeSuccess = False
                    , outcomeMessage = err
                    , outcomeBytesFreed = Nothing
                    , outcomeExpected = actionSizeEstimate action
                    , outcomePosition = position
                    , outcomeOrder = execOrder
                    }
              modifyIORef ref (`addEvent` ActionFailed outcome)
              case actionWikiRef action of
                Just wref -> recordOutcome cfg pgs (T.unpack wref) False
                Nothing   -> pure ()
        else do
          reason <- askSkipReason
          TIO.putStrLn "  Skipped."
          modifyIORef ref (`addEvent` ActionSkipped action reason)
      TIO.putStrLn ""

-- | Offer wiki contributions for approval and push. Returns paths that were pushed.
-- Supports batch approve (a) and batch skip (s) to avoid repetitive prompting.
offerLearn :: AppConfig -> IORef SessionLog -> [WikiPage] -> [WikiContribution] -> IO [FilePath]
offerLearn _ _ _ [] = do
  TIO.putStrLn "No wiki contributions suggested.\n"
  pure []
offerLearn config sessionRef pages contribs = do
  let deduped = deduplicateContribs pages contribs
      filtered = filter (\c -> not (T.isPrefixOf "_meta/" (T.pack (contribPath c)))) deduped
  TIO.putStrLn $ "-- " <> T.pack (show (length filtered))
              <> " wiki contribution(s) suggested --\n"
  -- Nothing = ask each, Just True = approve all, Just False = skip all
  batchRef <- newIORef (Nothing :: Maybe Bool)
  let totalCount = length filtered
  pushed <- mapM (\(idx, c) -> offerOne config sessionRef batchRef pages idx totalCount c)
                 (zip [0..] filtered)
  TIO.putStrLn ""
  pure [p | Just p <- pushed]
  where
    offerOne cfg ref batchRef pgs idx totalCount contrib = do
      let prefixed = contrib { contribSummary = prefixCommitMsg (contribSummary contrib) }
      TIO.putStrLn $ "  Type:    " <> T.pack (show (contribType prefixed))
      TIO.putStrLn $ "  Path:    " <> T.pack (contribPath prefixed)
      TIO.putStrLn $ "  Summary: " <> contribSummary prefixed
      TIO.putStrLn "  Preview:"
      let previewLines = take 10 (T.lines (contribContent prefixed))
      mapM_ (\l -> TIO.putStrLn $ "    " <> l) previewLines
      when (length (T.lines (contribContent prefixed)) > 10) $
        TIO.putStrLn "    ..."

      batchMode <- readIORef batchRef
      answer <- case batchMode of
        Just True  -> do
          TIO.putStrLn "  [auto-approve]"
          pure "y"
        Just False -> do
          TIO.putStrLn "  [auto-skip]"
          pure "n"
        Nothing -> do
          TIO.putStr "  Push to wiki? [y] yes  [n] skip  [e] edit first  [a] approve all  [s] skip all > "
          hFlush stdout
          getLine

      case answer of
        "a" -> do
          let remaining = totalCount - idx
          TIO.putStr $ "  Approve all " <> T.pack (show remaining)
                    <> " remaining contribution(s)? [y/n] > "
          hFlush stdout
          confirm <- getLine
          if confirm == "y"
            then do
              writeIORef batchRef (Just True)
              pushOne cfg ref pgs prefixed
            else do
              TIO.putStrLn "  Skipped (staying in per-item mode)."
              modifyIORef ref (`addEvent` ContribPushed prefixed ContribSkipped)
              pure Nothing
        "s" -> do
          writeIORef batchRef (Just False)
          TIO.putStrLn "  Skipped."
          modifyIORef ref (`addEvent` ContribPushed prefixed ContribSkipped)
          pure Nothing
        "y" -> pushOne cfg ref pgs prefixed
        "e" -> do
          TIO.putStrLn "  Editing is not yet supported in this terminal."
          TIO.putStrLn "  Pushing original content."
          result <- pushContribution cfg pgs prefixed
          case result of
            Right () -> do
              TIO.putStrLn "  OK: Contribution pushed to wiki."
              modifyIORef ref (`addEvent` ContribPushed prefixed
                (ContribEdited "user requested edit"))
              pure (Just (contribPath prefixed))
            Left err -> do
              TIO.putStrLn $ "  Error: " <> T.pack (show err)
              savePending prefixed
              modifyIORef ref (`addEvent` ContribFailed prefixed (T.pack (show err)))
              pure Nothing
        _ -> do
          TIO.putStrLn "  Skipped."
          modifyIORef ref (`addEvent` ContribPushed prefixed ContribSkipped)
          pure Nothing

    pushOne cfg ref pgs prefixed = do
      result <- pushContribution cfg pgs prefixed
      case result of
        Right () -> do
          TIO.putStrLn "  OK: Contribution pushed to wiki."
          modifyIORef ref (`addEvent` ContribPushed prefixed ContribApproved)
          pure (Just (contribPath prefixed))
        Left err -> do
          TIO.putStrLn $ "  Error: " <> T.pack (show err)
          TIO.putStrLn "  Saving to ~/.diskwise/pending/ for next session."
          savePending prefixed
          modifyIORef ref (`addEvent` ContribFailed prefixed (T.pack (show err)))
          pure Nothing

-- | Retry pending contributions from previous sessions
retryPending :: AppConfig -> IO ()
retryPending config = do
  home <- getHomeDirectory
  let pendingDir = home <> "/.diskwise/pending"
  exists <- doesDirectoryExist pendingDir
  if not exists
    then pure ()
    else do
      files <- listDirectory pendingDir
        `catch` (\(_ :: SomeException) -> pure [])
      unless (null files) $ do
        TIO.putStrLn $ "Retrying " <> T.pack (show (length files))
                    <> " pending contribution(s) from previous session..."
        wikiPages <- fetchWikiGracefully config
        mapM_ (retryOne config wikiPages pendingDir) files
        TIO.putStrLn ""

-- | Retry a single pending contribution file
retryOne :: AppConfig -> [WikiPage] -> FilePath -> FilePath -> IO ()
retryOne config pages pendingDir filename = do
  let fullPath = pendingDir <> "/" <> filename
      wikiPath = map (\c -> if c == '_' then '/' else c) filename
  content <- TIO.readFile fullPath
    `catch` (\(_ :: SomeException) -> pure "")
  if T.null content
    then pure ()
    else do
      let contrib = WikiContribution
            { contribType    = CreatePage
            , contribPath    = wikiPath
            , contribContent = content
            , contribSummary = prefixCommitMsg ("retry pending: " <> T.pack wikiPath)
            }
      result <- pushContribution config pages contrib
      case result of
        Right () -> do
          TIO.putStrLn $ "  Retried OK: " <> T.pack wikiPath
          removeFile fullPath
        Left err ->
          TIO.putStrLn $ "  Still failing: " <> T.pack wikiPath
                      <> " — " <> T.pack (show err)

-- | Save a failed contribution locally for retry
savePending :: WikiContribution -> IO ()
savePending contrib = do
  home <- getHomeDirectory
  let pendingDir = home <> "/.diskwise/pending"
  createDirectoryIfMissing True pendingDir
  let filename = pendingDir <> "/" <> sanitizeFilename (contribPath contrib)
  TIO.writeFile filename (contribContent contrib)
  where
    sanitizeFilename = map (\c -> if c == '/' then '_' else c)

-- | Ask for post-cleanup feedback if any actions were executed
postCleanupFeedback :: IORef SessionLog -> IO ()
postCleanupFeedback sessionRef = do
  session <- readIORef sessionRef
  let hasExecuted = any isExecuted (logEvents session)
  when hasExecuted $ do
    TIO.putStrLn "\n-- Post-cleanup check --\n"
    TIO.putStr "Did anything break or behave unexpectedly after cleanup?\n[n] Everything's fine  [y] Something went wrong\n> "
    hFlush stdout
    answer <- getLine
    case answer of
      "y" -> do
        TIO.putStr "Briefly describe what happened: "
        hFlush stdout
        feedback <- getLine
        modifyIORef sessionRef (`addEvent` UserFeedback (T.pack feedback))
      _ -> pure ()
  where
    isExecuted (ActionExecuted _) = True
    isExecuted _                  = False

-- | Check whether previously cleaned paths have regrown since last session
checkRegrowth :: Maybe SessionSummary -> IO T.Text
checkRegrowth Nothing = pure ""
checkRegrowth (Just summary) = do
  let cleaned = summaryCleanedPaths summary
  if null cleaned
    then pure ""
    else do
      TIO.putStrLn "-- Checking for regrowth from last session --\n"
      reports <- mapM checkOne cleaned
      let regrown = filter (not . T.null) reports
      if null regrown
        then do
          TIO.putStrLn "  No significant regrowth detected.\n"
          pure ""
        else do
          mapM_ TIO.putStrLn regrown
          TIO.putStrLn ""
          let report = T.unlines $
                [ "== REGROWTH REPORT =="
                , "The following paths cleaned in the previous session have regrown:"
                ] <> regrown <>
                [ ""
                , "Consider whether these paths need recurring cleanup automation."
                ]
          pure report
  where
    checkOne (path, freedBefore) = do
      currentSize <- measurePathSize path
      case currentSize of
        Just sz | sz > freedBefore `div` 2 ->
          pure $ "  Regrowth: " <> T.pack path <> " is now "
            <> formatSize sz <> " (was freed " <> formatSize freedBefore <> ")"
        _ -> pure ""
    formatSize b
      | b >= 1024 * 1024 * 1024 = T.pack (show (b `div` (1024 * 1024 * 1024))) <> " GB"
      | b >= 1024 * 1024        = T.pack (show (b `div` (1024 * 1024))) <> " MB"
      | otherwise               = T.pack (show (b `div` 1024)) <> " KB"

-- | Ask about delayed side effects from the previous session
askDelayedFeedback :: IORef SessionLog -> Maybe SessionSummary -> IO ()
askDelayedFeedback _ Nothing = pure ()
askDelayedFeedback sessionRef (Just summary) = do
  let hadActions = summaryActionsRun summary > 0
  when hadActions $ do
    TIO.putStrLn "-- Follow-up on previous session --\n"
    TIO.putStr "Did anything break after last session's cleanup?\n[n] No  [y] Yes\n> "
    hFlush stdout
    answer <- getLine
    case answer of
      "y" -> do
        TIO.putStr "Briefly describe: "
        hFlush stdout
        feedback <- getLine
        modifyIORef sessionRef (`addEvent` UserFeedback
          (T.pack ("delayed issue from previous session: " <> feedback)))
      _ -> pure ()
    TIO.putStrLn ""

-- | Ask the user why they skipped a cleanup action
askSkipReason :: IO SkipReason
askSkipReason = do
  TIO.putStr "  Why skip? [r] too risky  [l] later  [d] already done  [n] doesn't apply  [enter] no reason > "
  hFlush stdout
  answer <- getLine
  pure $ case answer of
    "r" -> TooRisky
    "l" -> NotNow
    "d" -> AlreadyHandled
    "n" -> NotApplicable
    ""  -> NotNow
    s   -> SkipReasonOther (T.pack s)

-- helpers

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = pure ()

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

