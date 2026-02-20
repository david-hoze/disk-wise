{-# LANGUAGE OverloadedStrings #-}

module DiskWise.HistorySpec (spec) where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.IO (hPutStrLn, hClose, openTempFile)
import Test.Hspec

import DiskWise.Types
import DiskWise.History (summarizeSession, formatSessionHistory, computeSkipPatterns)

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2025 1 15) 43200

spec :: Spec
spec = do
  describe "summarizeSession" $ do
    it "produces correct counts from a mock session log" $ do
      let action1 = CleanupAction "Clean npm" "npm cache clean" "low" (Just "~500 MB") Nothing
          action2 = CleanupAction "Remove logs" "rm -rf /var/log/old" "medium" Nothing Nothing
          outcome1 = CleanupOutcome action1 True "done" (Just 524288000) (Just "~500 MB") 0 0
          outcome2 = CleanupOutcome action2 False "permission denied" Nothing Nothing 1 1
          session = emptySessionLog
            { logFindings = [Finding "/tmp/x" 1000 "temp" "temp file"]
            , logPlatform = PlatformInfo "linux" "x86_64" "bash"
            , logEvents =
                [ ActionExecuted outcome1
                , ActionFailed outcome2
                , ActionSkipped action1 TooRisky
                , UserFeedback "docker broke"
                ]
            }
      summary <- summarizeSession session
      summaryFindingCount summary `shouldBe` 1
      summaryActionsRun summary `shouldBe` 1
      summaryActionsFailed summary `shouldBe` 1
      summaryBytesFreed summary `shouldBe` Just 524288000
      summaryUserFeedback summary `shouldBe` Just "docker broke"
      length (summarySkipReasons summary) `shouldBe` 1
      length (summaryFailedCmds summary) `shouldBe` 1

    it "handles empty session log" $ do
      summary <- summarizeSession emptySessionLog
      summaryFindingCount summary `shouldBe` 0
      summaryActionsRun summary `shouldBe` 0
      summaryBytesFreed summary `shouldBe` Nothing

  describe "formatSessionHistory" $ do
    it "returns empty string for no history" $ do
      formatSessionHistory [] `shouldBe` ""

    it "formats history with session data" $ do
      let summary = SessionSummary
            { summaryTimestamp     = testTime
            , summaryPlatform      = PlatformInfo "linux" "x86_64" "bash"
            , summaryFindingCount  = 5
            , summaryActionsRun    = 3
            , summaryActionsFailed = 1
            , summarySkipReasons   = [("Remove dist-newstyle", TooRisky)]
            , summaryBytesFreed    = Just (500 * 1024 * 1024)
            , summaryUserFeedback  = Nothing
            , summaryFailedCmds    = []
            }
          formatted = formatSessionHistory [summary]
      formatted `shouldSatisfy` (/= "")

  describe "computeSkipPatterns" $ do
    it "aggregates skip reasons across sessions" $ do
      let summary1 = SessionSummary
            { summaryTimestamp     = testTime
            , summaryPlatform      = PlatformInfo "linux" "x86_64" "bash"
            , summaryFindingCount  = 5
            , summaryActionsRun    = 1
            , summaryActionsFailed = 0
            , summarySkipReasons   = [("Remove dist-newstyle", TooRisky)]
            , summaryBytesFreed    = Nothing
            , summaryUserFeedback  = Nothing
            , summaryFailedCmds    = []
            }
          summary2 = summary1
            { summarySkipReasons = [ ("Remove dist-newstyle", TooRisky)
                                   , ("Clear journal", NotApplicable) ]
            }
          patterns = computeSkipPatterns [summary1, summary2]
      length patterns `shouldBe` 2
      -- dist-newstyle should be skipped 2 times
      case filter (\(d, _, _) -> d == "Remove dist-newstyle") patterns of
        [(_, count, reasons)] -> do
          count `shouldBe` 2
          length reasons `shouldBe` 2
        _ -> expectationFailure "Expected one pattern for dist-newstyle"

    it "returns empty for no skips" $ do
      let summary = SessionSummary
            { summaryTimestamp = testTime
            , summaryPlatform = PlatformInfo "linux" "x86_64" "bash"
            , summaryFindingCount = 0, summaryActionsRun = 0, summaryActionsFailed = 0
            , summarySkipReasons = [], summaryBytesFreed = Nothing
            , summaryUserFeedback = Nothing, summaryFailedCmds = []
            }
      computeSkipPatterns [summary] `shouldBe` []
