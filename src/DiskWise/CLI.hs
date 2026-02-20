{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.CLI
  ( runApp
  , runInvestigate
  , deduplicateContribs
  ) where

import Control.Exception (catch, SomeException)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory,
                         listDirectory, doesDirectoryExist, removeFile)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))

import DiskWise.Types
import DiskWise.Wiki
import DiskWise.Claude (investigate, callClaude, buildSystemPrompt, buildLearnPrompt,
                        parseAdvice, agentIdentity, prefixCommitMsg)
import DiskWise.Scanner

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
    _   -> do
      TIO.putStrLn "Unknown option, try again."
      mainLoop config

-- | Full investigation flow: scan -> parse -> wiki -> match -> Claude -> act -> learn -> refactor
runInvestigate :: AppConfig -> IO ()
runInvestigate config = do
  sessionRef <- newIORef emptySessionLog
  identity <- agentIdentity

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

  -- Step 4: Match pages to findings (Claude-assisted, with heuristic fallback)
  matched <- matchPagesWithClaude (callClaude config) wikiPages findings
  let matchedFindingPaths = concatMap (map findingPath . snd) matched
      novelFindings = filter (\f -> findingPath f `notElem` matchedFindingPaths) findings

  TIO.putStrLn $ "Wiki matched " <> T.pack (show (length matched))
              <> " page(s) to findings."
  TIO.putStrLn $ T.pack (show (length novelFindings))
              <> " finding(s) not covered by wiki.\n"

  -- Step 5: Call Claude for investigation
  TIO.putStrLn "-- Asking Claude to analyze --\n"
  result <- investigate config scanOutput matched novelFindings
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> T.pack (show err)
    Right advice -> do
      modifyIORef sessionRef (\s -> s { logAdvice = Just advice })

      -- Step 6: Present analysis
      presentAdvice advice

      -- Step 7: Offer cleanup actions (tracking results in session)
      offerCleanup config sessionRef wikiPages (adviceCleanupActions advice)

      -- Step 8: Session-aware learning — ask Claude to review the whole session
      TIO.putStrLn "-- Learning from session --\n"
      session <- readIORef sessionRef
      learnResult <- callClaude config buildSystemPrompt (buildLearnPrompt session identity)
      let allContribs = case learnResult of
            Right text -> case parseAdvice text of
              Right learnAdvice -> adviceContributions advice <> adviceContributions learnAdvice
              Left _ -> adviceContributions advice
            Left _ -> adviceContributions advice

      -- Step 9: Offer wiki contributions
      _ <- offerLearn config sessionRef wikiPages allContribs
      pure ()

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
  mapM_ (offerOne config sessionRef pages) actions
  TIO.putStrLn ""
  where
    offerOne cfg ref pgs action = do
      TIO.putStrLn $ "  Action:   " <> actionDescription action
      TIO.putStrLn $ "  Command:  " <> actionCommand action
      TIO.putStrLn $ "  Risk:     " <> actionRiskLevel action
      case actionSizeEstimate action of
        Just est -> TIO.putStrLn $ "  Estimate: " <> est
        Nothing  -> pure ()
      case actionWikiRef action of
        Just wref -> TIO.putStrLn $ "  Wiki ref: " <> wref
        Nothing  -> pure ()

      TIO.putStr "  Execute? [y/n] > "
      hFlush stdout
      answer <- getLine
      case answer of
        "y" -> do
          result <- runCleanupAction action
          case result of
            Right msg -> do
              TIO.putStrLn $ "  OK: " <> msg
              modifyIORef ref (`addEvent` ActionExecuted action msg)
              case actionWikiRef action of
                Just wref -> recordOutcome cfg pgs (T.unpack wref) True
                Nothing   -> pure ()
            Left err -> do
              TIO.putStrLn $ "  Error: " <> err
              modifyIORef ref (`addEvent` ActionFailed action err)
              case actionWikiRef action of
                Just wref -> recordOutcome cfg pgs (T.unpack wref) False
                Nothing   -> pure ()
        _ -> do
          TIO.putStrLn "  Skipped."
          modifyIORef ref (`addEvent` ActionSkipped action)
      TIO.putStrLn ""

-- | Offer wiki contributions for approval and push. Returns paths that were pushed.
offerLearn :: AppConfig -> IORef SessionLog -> [WikiPage] -> [WikiContribution] -> IO [FilePath]
offerLearn _ _ _ [] = do
  TIO.putStrLn "No wiki contributions suggested.\n"
  pure []
offerLearn config sessionRef pages contribs = do
  let deduped = deduplicateContribs pages contribs
      filtered = filter (\c -> not (T.isPrefixOf "_meta/" (T.pack (contribPath c)))) deduped
  TIO.putStrLn $ "-- " <> T.pack (show (length filtered))
              <> " wiki contribution(s) suggested --\n"
  pushed <- mapM (offerOne config sessionRef pages) filtered
  TIO.putStrLn ""
  pure [p | Just p <- pushed]
  where
    offerOne cfg ref pgs contrib = do
      let prefixed = contrib { contribSummary = prefixCommitMsg (contribSummary contrib) }
      TIO.putStrLn $ "  Type:    " <> T.pack (show (contribType prefixed))
      TIO.putStrLn $ "  Path:    " <> T.pack (contribPath prefixed)
      TIO.putStrLn $ "  Summary: " <> contribSummary prefixed
      TIO.putStrLn "  Preview:"
      let previewLines = take 10 (T.lines (contribContent prefixed))
      mapM_ (\l -> TIO.putStrLn $ "    " <> l) previewLines
      when (length (T.lines (contribContent prefixed)) > 10) $
        TIO.putStrLn "    ..."

      TIO.putStr "  Push to wiki? [y/n] > "
      hFlush stdout
      answer <- getLine
      case answer of
        "y" -> do
          result <- pushContribution cfg pgs prefixed
          case result of
            Right () -> do
              TIO.putStrLn "  OK: Contribution pushed to wiki."
              modifyIORef ref (`addEvent` ContribPushed prefixed)
              pure (Just (contribPath prefixed))
            Left err -> do
              TIO.putStrLn $ "  Error: " <> T.pack (show err)
              TIO.putStrLn "  Saving to ~/.diskwise/pending/ for next session."
              savePending prefixed
              modifyIORef ref (`addEvent` ContribFailed prefixed (T.pack (show err)))
              pure Nothing
        _ -> do
          TIO.putStrLn "  Skipped."
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

-- helpers

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = pure ()

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

