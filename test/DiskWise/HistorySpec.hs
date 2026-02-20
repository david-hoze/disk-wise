{-# LANGUAGE OverloadedStrings #-}

module DiskWise.HistorySpec (spec) where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.IO (hPutStrLn, hClose, openTempFile)
import Test.Hspec

import DiskWise.Types
import DiskWise.History (summarizeSession, formatSessionHistory)

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2025 1 15) 43200

spec :: Spec
spec = do
  describe "summarizeSession" $ do
    it "produces correct counts from a mock session log" $ do
      let action1 = CleanupAction "Clean npm" "npm cache clean" "low" (Just "~500 MB") Nothing
          action2 = CleanupAction "Remove logs" "rm -rf /var/log/old" "medium" Nothing Nothing
          outcome1 = CleanupOutcome action1 True "done" (Just 524288000) (Just "~500 MB")
          outcome2 = CleanupOutcome action2 False "permission denied" Nothing Nothing
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
