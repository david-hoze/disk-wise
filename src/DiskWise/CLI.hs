{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.CLI
  ( runApp
  , runInvestigate
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))

import DiskWise.Types
import DiskWise.Wiki
import DiskWise.Claude
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

-- | Full investigation flow: scan -> parse -> fetch wiki -> match -> Claude -> act -> learn
runInvestigate :: AppConfig -> IO ()
runInvestigate config = do
  -- Step 1: Scan the system
  TIO.putStrLn "\n-- Scanning system --\n"
  scanOutput <- scanSystem config

  -- Step 2: Parse findings
  let findings = parseFindings scanOutput
  TIO.putStrLn $ "Found " <> T.pack (show (length findings)) <> " items of interest.\n"

  -- Step 3: Fetch wiki pages (graceful failure -> empty list)
  TIO.putStrLn "-- Fetching wiki knowledge --\n"
  wikiPages <- fetchWikiGracefully config

  -- Step 4: Match pages to findings
  let matched = matchPages wikiPages findings
      matchedFindingPaths = concatMap (map findingPath . snd) matched
      novelFindings = filter (\f -> findingPath f `notElem` matchedFindingPaths) findings

  TIO.putStrLn $ "Wiki matched " <> T.pack (show (length matched))
              <> " page(s) to findings."
  TIO.putStrLn $ T.pack (show (length novelFindings))
              <> " finding(s) not covered by wiki.\n"

  -- Step 5: Call Claude
  TIO.putStrLn "-- Asking Claude to analyze --\n"
  result <- investigate config scanOutput matched novelFindings
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> T.pack (show err)
    Right advice -> do
      -- Step 6: Present analysis
      presentAdvice advice

      -- Step 7: Offer cleanup actions
      offerCleanup (adviceCleanupActions advice)

      -- Step 8: Offer wiki contributions
      offerLearn config wikiPages (adviceContributions advice)

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

-- | Offer cleanup actions one by one, requiring confirmation
offerCleanup :: [CleanupAction] -> IO ()
offerCleanup [] = TIO.putStrLn "No cleanup actions suggested.\n"
offerCleanup actions = do
  TIO.putStrLn $ "-- " <> T.pack (show (length actions)) <> " cleanup action(s) suggested --\n"
  mapM_ offerOne actions
  TIO.putStrLn ""
  where
    offerOne action = do
      TIO.putStrLn $ "  Action:   " <> actionDescription action
      TIO.putStrLn $ "  Command:  " <> actionCommand action
      TIO.putStrLn $ "  Risk:     " <> actionRiskLevel action
      case actionSizeEstimate action of
        Just est -> TIO.putStrLn $ "  Estimate: " <> est
        Nothing  -> pure ()
      case actionWikiRef action of
        Just ref -> TIO.putStrLn $ "  Wiki ref: " <> ref
        Nothing  -> pure ()

      TIO.putStr "  Execute? [y/n] > "
      hFlush stdout
      answer <- getLine
      case answer of
        "y" -> do
          result <- runCleanupAction action
          case result of
            Right msg -> TIO.putStrLn $ "  OK: " <> msg
            Left err  -> TIO.putStrLn $ "  Error: " <> err
        _ -> TIO.putStrLn "  Skipped."
      TIO.putStrLn ""

-- | Offer wiki contributions for approval and push
offerLearn :: AppConfig -> [WikiPage] -> [WikiContribution] -> IO ()
offerLearn _ _ [] = TIO.putStrLn "No wiki contributions suggested.\n"
offerLearn config pages contribs = do
  TIO.putStrLn $ "-- " <> T.pack (show (length contribs))
              <> " wiki contribution(s) suggested --\n"
  mapM_ (offerOne config pages) contribs
  TIO.putStrLn ""
  where
    offerOne cfg pgs contrib = do
      TIO.putStrLn $ "  Type:    " <> T.pack (show (contribType contrib))
      TIO.putStrLn $ "  Path:    " <> T.pack (contribPath contrib)
      TIO.putStrLn $ "  Summary: " <> contribSummary contrib
      TIO.putStrLn "  Preview:"
      let previewLines = take 10 (T.lines (contribContent contrib))
      mapM_ (\l -> TIO.putStrLn $ "    " <> l) previewLines
      when (length (T.lines (contribContent contrib)) > 10) $
        TIO.putStrLn "    ..."

      TIO.putStr "  Push to wiki? [y/n] > "
      hFlush stdout
      answer <- getLine
      case answer of
        "y" -> do
          result <- pushContribution cfg pgs contrib
          case result of
            Right () -> TIO.putStrLn "  OK: Contribution pushed to wiki."
            Left err -> do
              TIO.putStrLn $ "  Error: " <> T.pack (show err)
              TIO.putStrLn "  Saving to ~/.diskwise/pending/ for next session."
              savePending contrib
        _ -> TIO.putStrLn "  Skipped."
      TIO.putStrLn ""

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

-- | Control.Monad.when (re-implemented to avoid import)
when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = pure ()
