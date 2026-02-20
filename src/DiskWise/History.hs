{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.History
  ( saveSessionSummary
  , loadSessionHistory
  , loadMostRecentSummary
  , summarizeSession
  , formatSessionHistory
  , computeSkipPatterns
  , computeCommandStats
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)

import DiskWise.Types

-- | Append a session summary to ~/.diskwise/session-history.jsonl
saveSessionSummary :: SessionSummary -> IO ()
saveSessionSummary summary = do
  home <- getHomeDirectory
  let dir = home <> "/.diskwise"
      file = dir <> "/session-history.jsonl"
  createDirectoryIfMissing True dir
  BL.appendFile file (encode summary <> "\n")
  `catch` (\(_ :: SomeException) -> pure ())

-- | Load all session summaries from the history file.
-- Returns [] on missing or corrupt file.
loadSessionHistory :: IO [SessionSummary]
loadSessionHistory = do
  home <- getHomeDirectory
  let file = home <> "/.diskwise/session-history.jsonl"
  exists <- doesFileExist file
  if not exists
    then pure []
    else do
      content <- BLC.readFile file
        `catch` (\(_ :: SomeException) -> pure "")
      let lines' = filter (not . BLC.null) (BLC.lines content)
      pure [s | line <- lines', Right s <- [eitherDecode line]]

-- | Extract a summary from a full session log
summarizeSession :: SessionLog -> IO SessionSummary
summarizeSession sessionLog = do
  now <- getCurrentTime
  pure SessionSummary
    { summaryTimestamp     = now
    , summaryPlatform      = logPlatform sessionLog
    , summaryFindingCount  = length (logFindings sessionLog)
    , summaryActionsRun    = length [() | ActionExecuted _ <- logEvents sessionLog]
    , summaryActionsFailed = length [() | ActionFailed _ <- logEvents sessionLog]
    , summarySkipReasons   = [ (actionDescription (act), reason)
                             | ActionSkipped act reason <- logEvents sessionLog ]
    , summaryBytesFreed    = sumBytesFreed (logEvents sessionLog)
    , summaryUserFeedback  = lastFeedback (logEvents sessionLog)
    , summaryFailedCmds    = [ (actionCommand (outcomeAction o), outcomeMessage o)
                             | ActionFailed o <- logEvents sessionLog ]
    , summaryCleanedPaths  = [ (path, b)
                             | ActionExecuted o <- logEvents sessionLog
                             , outcomeSuccess o
                             , Just b <- [outcomeBytesFreed o]
                             , let path = extractPath (actionCommand (outcomeAction o))
                             , not (null path)
                             ]
    , summarySucceededCmds = [ actionCommand (outcomeAction o)
                             | ActionExecuted o <- logEvents sessionLog
                             , outcomeSuccess o
                             ]
    }

-- | Sum all bytes freed across executed actions
sumBytesFreed :: [SessionEvent] -> Maybe Integer
sumBytesFreed events =
  let freed = [b | ActionExecuted o <- events, Just b <- [outcomeBytesFreed o]]
  in if null freed then Nothing else Just (sum freed)

-- | Get the last user feedback from the session
lastFeedback :: [SessionEvent] -> Maybe T.Text
lastFeedback events =
  case [t | UserFeedback t <- events] of
    [] -> Nothing
    xs -> Just (last xs)

-- | Format session history as a summary for inclusion in Claude prompts
formatSessionHistory :: [SessionSummary] -> T.Text
formatSessionHistory [] = ""
formatSessionHistory summaries =
  let recent = take 10 (reverse summaries)  -- most recent first
      totalSessions = length summaries
      totalFreed = sum [b | s <- summaries, Just b <- [summaryBytesFreed s]]
      avgFreed = if totalSessions > 0
                 then totalFreed `div` fromIntegral totalSessions
                 else 0
      skipPatterns = computeSkipPatterns summaries
  in T.unlines $
    [ "== HISTORICAL PATTERNS (last " <> T.pack (show (length recent))
      <> " of " <> T.pack (show totalSessions) <> " sessions) =="
    ] <> map formatSummary recent <>
    [ ""
    , "Average space freed per session: ~" <> formatSize avgFreed
    ] <>
    (if null skipPatterns then [] else
      [ ""
      , "== SKIP PATTERNS =="
      , "The following actions are frequently skipped by users:"
      ] <> map formatSkipPattern skipPatterns <>
      [ ""
      , "SKIP PATTERNS INDICATE MISCALIBRATED ADVICE. If an action is skipped more"
      , "than half the time:"
      , "- If the reason is \"too risky\": amend the wiki page to raise the documented"
      , "  risk level, or add caveats to \"What's NOT safe to delete\"."
      , "- If the reason is \"not applicable\": the matching may be wrong, or the page"
      , "  needs better platform notes."
      , "- If the reason is \"already handled\": this is fine, no wiki change needed."
      , "- If the reason is \"not now\": this is fine, no wiki change needed."
      ]
    )
  where
    formatSummary s = T.unlines $
      [ "- Session on " <> platformOS (summaryPlatform s) <> "/" <> platformArch (summaryPlatform s)
        <> ": " <> T.pack (show (summaryFindingCount s)) <> " findings, "
        <> T.pack (show (summaryActionsRun s)) <> " actions run, "
        <> T.pack (show (summaryActionsFailed s)) <> " failed"
        <> maybe "" (\b -> ", freed " <> formatSize b) (summaryBytesFreed s)
      ] <> [ "  Feedback: \"" <> fb <> "\""
           | Just fb <- [summaryUserFeedback s] ]
        <> [ "  Failed: " <> cmd <> " — " <> err
           | (cmd, err) <- summaryFailedCmds s ]
        <> [ "  Skipped: " <> desc <> " (" <> reasonText r <> ")"
           | (desc, r) <- summarySkipReasons s ]

    formatSize b
      | b >= 1024 * 1024 * 1024 = T.pack (show (b `div` (1024 * 1024 * 1024))) <> " GB"
      | b >= 1024 * 1024        = T.pack (show (b `div` (1024 * 1024))) <> " MB"
      | otherwise               = T.pack (show (b `div` 1024)) <> " KB"

    formatSkipPattern (desc, count, reasons) =
      "- \"" <> desc <> "\" skipped " <> T.pack (show count) <> " time(s)"
      <> "\n  Reasons: " <> T.intercalate ", " (map reasonText reasons)

    reasonText TooRisky            = "too risky"
    reasonText NotNow              = "not now"
    reasonText AlreadyHandled      = "already handled"
    reasonText NotApplicable       = "not applicable"
    reasonText (SkipReasonOther t) = t

-- | Compute skip patterns across sessions.
-- Returns: (action description, times skipped, skip reasons)
computeSkipPatterns :: [SessionSummary] -> [(T.Text, Int, [SkipReason])]
computeSkipPatterns summaries =
  let skipMap = Map.fromListWith (++)
        [ (desc, [reason])
        | s <- summaries
        , (desc, reason) <- summarySkipReasons s
        ]
  in [ (desc, length reasons, reasons)
     | (desc, reasons) <- Map.toList skipMap ]

-- | Load just the most recent session summary, if any.
loadMostRecentSummary :: IO (Maybe SessionSummary)
loadMostRecentSummary = do
  history <- loadSessionHistory
  pure $ case history of
    [] -> Nothing
    _  -> Just (last history)

-- | Extract a path from a cleanup command.
-- Looks for tokens starting with / or ~ (the target path of the cleanup).
extractPath :: T.Text -> FilePath
extractPath cmd =
  let ws = T.words cmd
      paths = filter (\w -> T.isPrefixOf "/" w || T.isPrefixOf "~" w) ws
  in case paths of
    (p:_) -> T.unpack p
    []    -> ""

-- | Compute command reliability statistics across all sessions.
computeCommandStats :: [SessionSummary] -> [CommandStats]
computeCommandStats summaries =
  let -- Collect all successes and failures per command
      successes = Map.fromListWith (+)
        [ (cmd, 1 :: Int)
        | s <- summaries
        , cmd <- summarySucceededCmds s
        ]
      failures = Map.fromListWith (\new old -> new) -- keep most recent error
        [ (cmd, (1 :: Int, err))
        | s <- summaries
        , (cmd, err) <- summaryFailedCmds s
        ]
      failCounts = Map.fromListWith (+)
        [ (cmd, 1 :: Int)
        | s <- summaries
        , (cmd, _) <- summaryFailedCmds s
        ]
      allCmds = Map.keys (Map.union successes failCounts)
  in [ CommandStats
        { cmdStatsCommand   = cmd
        , cmdStatsSuccesses = Map.findWithDefault 0 cmd successes
        , cmdStatsFailures  = Map.findWithDefault 0 cmd failCounts
        , cmdStatsLastError = fmap snd (Map.lookup cmd failures)
        }
     | cmd <- allCmds
     , Map.findWithDefault 0 cmd successes + Map.findWithDefault 0 cmd failCounts >= 2
     ]
