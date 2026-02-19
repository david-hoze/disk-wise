{-# LANGUAGE OverloadedStrings #-}

module DiskWise.ScannerSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Scanner (parseFindings, toMingwPath)

spec :: Spec
spec = do
  describe "parseFindings" $ do
    it "parses du-style output lines" $ do
      let output = T.unlines
            [ "=== Largest directories ==="
            , "1.2G\t/home/user/.npm"
            , "500M\t/home/user/.cache/pip"
            , ""
            ]
          findings = parseFindings output
      length findings `shouldBe` 2
      findingPath (head findings) `shouldBe` "/home/user/.npm"
      findingCategory (head findings) `shouldBe` "package-manager"

    it "categorizes paths correctly" $ do
      let output = T.unlines
            [ "200M\t/home/user/.cache/something"
            , "100M\t/var/log/syslog"
            , "300M\t/home/user/project/dist-newstyle"
            , "50M\t/tmp/scratch"
            ]
          findings = parseFindings output
          cats = map findingCategory findings
      cats `shouldBe` ["cache", "log", "build-artifact", "temp"]

    it "skips section headers and blank lines" $ do
      let output = T.unlines
            [ "=== Some Section ==="
            , ""
            , "100M\t/some/path"
            , ""
            ]
          findings = parseFindings output
      length findings `shouldBe` 1

    it "returns empty list for unparseable input" $ do
      let output = "This is just text with no tab-separated sizes\n"
          findings = parseFindings output
      findings `shouldBe` []

    it "parses various size suffixes" $ do
      let output = T.unlines
            [ "15K\t/small/file"
            , "2.5G\t/big/file"
            , "1T\t/huge/file"
            ]
          findings = parseFindings output
      length findings `shouldBe` 3
      findingSize (findings !! 0) `shouldBe` round (15 * 1024 :: Double)
      findingSize (findings !! 1) `shouldBe` round (2.5 * 1024 * 1024 * 1024 :: Double)

  describe "toMingwPath" $ do
    it "converts Windows backslash paths" $ do
      toMingwPath "C:\\Users\\natanh" `shouldBe` "/c/Users/natanh"

    it "converts Windows forward-slash paths" $ do
      toMingwPath "C:/Users/natanh" `shouldBe` "/c/Users/natanh"

    it "lowercases the drive letter" $ do
      toMingwPath "D:\\Data" `shouldBe` "/d/Data"

    it "leaves MINGW paths unchanged" $ do
      toMingwPath "/c/Users/natanh" `shouldBe` "/c/Users/natanh"

    it "leaves Unix paths unchanged" $ do
      toMingwPath "/home/user" `shouldBe` "/home/user"
