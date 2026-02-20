{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.History
  ( saveSessionSummary
  , loadSessionHistory
  , summarizeSession
  , formatSessionHistory
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
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
  in T.unlines $
    [ "== HISTORICAL PATTERNS (last " <> T.pack (show (length recent))
      <> " of " <> T.pack (show totalSessions) <> " sessions) =="
    ] <> map formatSummary recent <>
    [ ""
    , "Average space freed per session: ~" <> formatSize avgFreed
    ]
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

    reasonText TooRisky            = "too risky"
    reasonText NotNow              = "not now"
    reasonText AlreadyHandled      = "already handled"
    reasonText NotApplicable       = "not applicable"
    reasonText (SkipReasonOther t) = t
