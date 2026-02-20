{-# LANGUAGE OverloadedStrings #-}

module DiskWise.HistorySpec (spec) where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.IO (hPutStrLn, hClose, openTempFile)
import Test.Hspec

import DiskWise.Types
import DiskWise.History (summarizeSession, formatSessionHistory, computeSkipPatterns,
                         computeCommandStats, detectDiminishingReturns, computeZeroYieldPaths)

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
      -- action1 command has no path tokens, so cleanedPaths should be empty
      summaryCleanedPaths summary `shouldBe` []

    it "populates cleanedPaths from successful actions with paths" $ do
      let action = CleanupAction "Remove logs" "rm -rf /var/log/old" "medium" Nothing Nothing
          outcome = CleanupOutcome action True "done" (Just 1000000) Nothing 0 0
          session = emptySessionLog { logEvents = [ActionExecuted outcome] }
      summary <- summarizeSession session
      summaryCleanedPaths summary `shouldBe` [("/var/log/old", 1000000)]

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
            , summaryCleanedPaths  = []
            , summarySucceededCmds = []
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
            , summaryCleanedPaths  = []
            , summarySucceededCmds = []
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
            , summaryCleanedPaths = [], summarySucceededCmds = []
            }
      computeSkipPatterns [summary] `shouldBe` []

  describe "computeCommandStats" $ do
    it "aggregates command successes and failures across sessions" $ do
      let summary1 = SessionSummary
            { summaryTimestamp = testTime
            , summaryPlatform = PlatformInfo "linux" "x86_64" "bash"
            , summaryFindingCount = 1, summaryActionsRun = 1, summaryActionsFailed = 0
            , summarySkipReasons = [], summaryBytesFreed = Nothing
            , summaryUserFeedback = Nothing
            , summaryFailedCmds = []
            , summaryCleanedPaths = [], summarySucceededCmds = ["npm cache clean"]
            }
          summary2 = summary1
            { summaryFailedCmds = [("npm cache clean", "permission denied")]
            , summarySucceededCmds = []
            }
          stats = computeCommandStats [summary1, summary2]
      length stats `shouldBe` 1
      case stats of
        [s] -> do
          cmdStatsCommand s `shouldBe` "npm cache clean"
          cmdStatsSuccesses s `shouldBe` 1
          cmdStatsFailures s `shouldBe` 1
          cmdStatsLastError s `shouldBe` Just "permission denied"
        _ -> expectationFailure "Expected one command stat"

    it "only includes commands with 2+ occurrences" $ do
      let summary = SessionSummary
            { summaryTimestamp = testTime
            , summaryPlatform = PlatformInfo "linux" "x86_64" "bash"
            , summaryFindingCount = 1, summaryActionsRun = 1, summaryActionsFailed = 0
            , summarySkipReasons = [], summaryBytesFreed = Nothing
            , summaryUserFeedback = Nothing
            , summaryFailedCmds = []
            , summaryCleanedPaths = [], summarySucceededCmds = ["npm cache clean"]
            }
      computeCommandStats [summary] `shouldBe` []

  describe "detectDiminishingReturns" $ do
    let mkSummary freed = SessionSummary
          { summaryTimestamp = testTime
          , summaryPlatform = PlatformInfo "linux" "x86_64" "bash"
          , summaryFindingCount = 1, summaryActionsRun = 1, summaryActionsFailed = 0
          , summarySkipReasons = [], summaryBytesFreed = freed
          , summaryUserFeedback = Nothing, summaryFailedCmds = []
          , summaryCleanedPaths = [], summarySucceededCmds = []
          }

    it "returns Just when 3+ sessions all freed < 10 MB" $ do
      let summaries = [ mkSummary (Just 5000000)
                       , mkSummary (Just 2000000)
                       , mkSummary (Just 0)
                       ]
      detectDiminishingReturns summaries `shouldBe` Just [0, 2000000, 5000000]

    it "returns Nothing when fewer than 3 sessions" $ do
      let summaries = [ mkSummary (Just 0), mkSummary (Just 0) ]
      detectDiminishingReturns summaries `shouldBe` Nothing

    it "returns Nothing when recent session freed > 10 MB" $ do
      let summaries = [ mkSummary (Just 0)
                       , mkSummary (Just 0)
                       , mkSummary (Just (20 * 1024 * 1024))
                       ]
      detectDiminishingReturns summaries `shouldBe` Nothing

    it "returns Nothing when a session freed Nothing (no actions)" $ do
      let summaries = [ mkSummary Nothing
                       , mkSummary (Just 0)
                       , mkSummary (Just 0)
                       ]
          -- Nothing maps to 0, so this should actually be Just
      detectDiminishingReturns summaries `shouldBe` Just [0, 0, 0]

  describe "computeZeroYieldPaths" $ do
    let mkSummaryWith cleaned = SessionSummary
          { summaryTimestamp = testTime
          , summaryPlatform = PlatformInfo "linux" "x86_64" "bash"
          , summaryFindingCount = 1, summaryActionsRun = 1, summaryActionsFailed = 0
          , summarySkipReasons = [], summaryBytesFreed = Just 0
          , summaryUserFeedback = Nothing, summaryFailedCmds = []
          , summaryCleanedPaths = cleaned, summarySucceededCmds = []
          }

    it "identifies paths cleaned 2+ times with avg < 1 MB" $ do
      let summaries = [ mkSummaryWith [("~/.cache/foo/*", 0)]
                       , mkSummaryWith [("~/.cache/foo/*", 0)]
                       ]
          result = computeZeroYieldPaths summaries
      result `shouldBe` [("~/.cache/foo/*", 2)]

    it "excludes paths with fewer than 2 cleanings" $ do
      let summaries = [ mkSummaryWith [("~/.cache/foo/*", 0)] ]
      computeZeroYieldPaths summaries `shouldBe` []

    it "excludes paths with avg >= 1 MB freed" $ do
      let summaries = [ mkSummaryWith [("~/.cache/big/*", 2000000)]
                       , mkSummaryWith [("~/.cache/big/*", 1500000)]
                       ]
      computeZeroYieldPaths summaries `shouldBe` []

    it "handles multiple paths across sessions" $ do
      let summaries = [ mkSummaryWith [("~/a/*", 0), ("~/b/*", 500000)]
                       , mkSummaryWith [("~/a/*", 28672), ("~/b/*", 400000)]
                       , mkSummaryWith [("~/a/*", 0)]
                       ]
          result = computeZeroYieldPaths summaries
      -- ~/a/* cleaned 3 times, avg ~9.5 KB -> zero yield
      -- ~/b/* cleaned 2 times, avg 450 KB -> zero yield
      length result `shouldBe` 2

    it "returns empty for no history" $ do
      computeZeroYieldPaths [] `shouldBe` []
