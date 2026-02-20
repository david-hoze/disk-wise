{-# LANGUAGE OverloadedStrings #-}

module DiskWise.TypesSpec (spec) where

import Data.Aeson (encode, eitherDecode, ToJSON, FromJSON)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Test.Hspec

import DiskWise.Types

jsonRoundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
jsonRoundTrip val = eitherDecode (encode val) `shouldBe` Right val

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2025 1 15) 43200

spec :: Spec
spec = do
  describe "DiskEntry JSON round-trip" $
    it "disk entry" $
      jsonRoundTrip DiskEntry
        { entryPath    = "/home/user/big.iso"
        , entrySize    = 4700000000
        , entryIsDir   = False
        , entryModTime = testTime
        }

  describe "WikiPage JSON round-trip" $ do
    it "wiki page" $
      jsonRoundTrip WikiPage
        { pageRelPath      = "tools/npm.md"
        , pageTopic        = "npm"
        , pageTitle        = "npm cache cleanup"
        , pageBody         = "# npm\n\nRun `npm cache clean --force`\n"
        , pageSha          = "abc123def456"
        , pageLastVerified = Nothing
        , pageVerifyCount  = 0
        , pageFailCount    = 0
        }

    it "wiki page with metadata" $
      jsonRoundTrip WikiPage
        { pageRelPath      = "tools/npm.md"
        , pageTopic        = "npm"
        , pageTitle        = "npm cache cleanup"
        , pageBody         = "# npm\n\nRun `npm cache clean --force`\n"
        , pageSha          = "abc123def456"
        , pageLastVerified = Just testTime
        , pageVerifyCount  = 5
        , pageFailCount    = 1
        }

  describe "ContribType JSON round-trip" $ do
    it "CreatePage" $ jsonRoundTrip CreatePage
    it "AmendPage"  $ jsonRoundTrip AmendPage

  describe "WikiContribution JSON round-trip" $
    it "contribution" $
      jsonRoundTrip WikiContribution
        { contribType    = CreatePage
        , contribPath    = "tools/yarn.md"
        , contribContent = "# Yarn\n\nClean with `yarn cache clean`\n"
        , contribSummary = "Add yarn cache cleanup page"
        }

  describe "CleanupAction JSON round-trip" $ do
    it "with all fields" $
      jsonRoundTrip CleanupAction
        { actionDescription  = "Clean npm cache"
        , actionCommand      = "npm cache clean --force"
        , actionRiskLevel    = "low"
        , actionSizeEstimate = Just "~500 MB"
        , actionWikiRef      = Just "tools/npm.md"
        }

    it "with optional fields as Nothing" $
      jsonRoundTrip CleanupAction
        { actionDescription  = "Remove old logs"
        , actionCommand      = "rm -rf /var/log/old/*.log"
        , actionRiskLevel    = "medium"
        , actionSizeEstimate = Nothing
        , actionWikiRef      = Nothing
        }

  describe "ClaudeAdvice JSON round-trip" $
    it "full advice" $
      jsonRoundTrip ClaudeAdvice
        { adviceAnalysis       = "Your system has 5GB of stale caches."
        , adviceCleanupActions =
            [ CleanupAction "Clean npm" "npm cache clean --force" "low"
                (Just "~500 MB") (Just "tools/npm.md")
            ]
        , adviceContributions  =
            [ WikiContribution CreatePage "tools/yarn.md"
                "# Yarn\nClean yarn cache" "Add yarn page"
            ]
        }

  describe "Finding JSON round-trip" $
    it "finding" $
      jsonRoundTrip Finding
        { findingPath     = "/home/user/.cache/pip"
        , findingSize     = 1073741824
        , findingCategory = "cache"
        , findingSummary  = "pip cache is 1GB"
        }

  describe "AppConfig JSON round-trip" $
    it "config" $
      jsonRoundTrip AppConfig
        { configApiKey     = "sk-ant-test"
        , configWikiOwner  = "myorg"
        , configWikiRepo   = "diskwise-wiki"
        , configWikiToken  = "ghp_test123"
        , configScanPaths  = ["/home/user", "/var"]
        , configMinSizeMB  = 50
        , configModel      = "claude-sonnet-4-20250514"
        , configGistId     = ""
        }

  describe "RefactorResult JSON round-trip" $
    it "refactor result" $
      jsonRoundTrip RefactorResult
        { refactorContributions =
            [ WikiContribution AmendPage "tools/npm.md"
                "# npm (improved)" "improve npm page"
            ]
        , refactorDone    = False
        , refactorSummary = "Improved npm page clarity"
        }

  describe "SessionLog" $ do
    it "starts empty" $ do
      logEvents emptySessionLog `shouldBe` []
      logScanOutput emptySessionLog `shouldBe` ""

    it "tracks events with addEvent" $ do
      let action = CleanupAction "test" "echo hi" "low" Nothing Nothing
          sl = emptySessionLog
                 `addEvent` ActionExecuted action "done"
                 `addEvent` ActionSkipped action
      length (logEvents sl) `shouldBe` 2
