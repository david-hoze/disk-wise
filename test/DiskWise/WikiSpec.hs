{-# LANGUAGE OverloadedStrings #-}

module DiskWise.WikiSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Wiki (matchPages, parsePagePatterns, parsePageToolNames, sanitizeContent)

-- | Helper to make a wiki page for testing
mkPage :: FilePath -> T.Text -> T.Text -> T.Text -> WikiPage
mkPage path topic title body = WikiPage
  { pageRelPath = path
  , pageTopic   = topic
  , pageTitle   = title
  , pageBody    = body
  , pageSha     = "abc123"
  }

-- | Helper to make a finding for testing
mkFinding :: FilePath -> T.Text -> T.Text -> Finding
mkFinding path cat summary = Finding
  { findingPath     = path
  , findingSize     = 1000000
  , findingCategory = cat
  , findingSummary  = summary
  }

spec :: Spec
spec = do
  describe "parsePagePatterns" $ do
    it "extracts backtick paths from markdown" $ do
      let body = T.unlines
            [ "# npm"
            , ""
            , "## Where it stores data"
            , "- `~/.npm/_cacache`"
            , "- `~/.npm/_logs`"
            , ""
            , "## Cleanup"
            , "Run `npm cache clean --force`"
            ]
      let patterns = parsePagePatterns body
      patterns `shouldSatisfy` any (T.isInfixOf ".npm")

    it "returns empty list for body without paths" $ do
      let body = "# Just a title\n\nNo paths here.\n"
      parsePagePatterns body `shouldBe` []

  describe "parsePageToolNames" $ do
    it "extracts topic and title" $ do
      let page = mkPage "tools/npm.md" "npm" "npm cache cleanup"
                   "# npm cache cleanup\n\n## Cleanup steps\n"
      let names = parsePageToolNames page
      names `shouldSatisfy` elem "npm"
      names `shouldSatisfy` elem "npm cache cleanup"

    it "includes h2 headings" $ do
      let page = mkPage "tools/docker.md" "docker" "Docker cleanup"
                   "# Docker cleanup\n\n## Dangling images\n\n## Build cache\n"
      let names = parsePageToolNames page
      names `shouldSatisfy` elem "dangling images"
      names `shouldSatisfy` elem "build cache"

  describe "matchPages" $ do
    it "matches pages to findings by path pattern" $ do
      let page = mkPage "tools/npm.md" "npm" "npm"
                   "# npm\n\n- `~/.npm`\n"
          findings =
            [ mkFinding "/home/user/.npm/_cacache" "cache" "npm cache"
            , mkFinding "/var/log/syslog" "log" "system log"
            ]
          result = matchPages [page] findings
      length result `shouldBe` 1
      case result of
        [(_, matched)] -> length matched `shouldBe` 1
        _              -> expectationFailure "Expected one match group"

    it "matches by tool name in category" $ do
      let page = mkPage "tools/docker.md" "docker" "Docker"
                   "# Docker\n\nClean up docker resources.\n"
          findings =
            [ mkFinding "/var/lib/docker" "docker" "docker images"
            ]
          result = matchPages [page] findings
      length result `shouldBe` 1

    it "returns empty when no matches" $ do
      let page = mkPage "tools/npm.md" "npm" "npm"
                   "# npm\n\n- `~/.npm`\n"
          findings =
            [ mkFinding "/var/log/syslog" "log" "system log"
            ]
      matchPages [page] findings `shouldBe` []

    it "matches multiple pages to their respective findings" $ do
      let npmPage = mkPage "tools/npm.md" "npm" "npm"
                      "# npm\n\n- `~/.npm`\n"
          dockerPage = mkPage "tools/docker.md" "docker" "Docker"
                         "# Docker\n\n- `/var/lib/docker`\n"
          findings =
            [ mkFinding "/home/user/.npm/_cacache" "cache" "npm cache"
            , mkFinding "/var/lib/docker/overlay2" "docker" "docker storage"
            , mkFinding "/tmp/random" "temp" "temp file"
            ]
          result = matchPages [npmPage, dockerPage] findings
      length result `shouldBe` 2
