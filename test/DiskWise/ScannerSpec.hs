{-# LANGUAGE OverloadedStrings #-}

module DiskWise.ScannerSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Scanner (parseFindings, toMingwPath, validateAction, ValidationResult(..))

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
          findings = parseFindings 0 output
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
          findings = parseFindings 0 output
          cats = map findingCategory findings
      cats `shouldBe` ["cache", "log", "build-artifact", "temp"]

    it "skips section headers and blank lines" $ do
      let output = T.unlines
            [ "=== Some Section ==="
            , ""
            , "100M\t/some/path"
            , ""
            ]
          findings = parseFindings 0 output
      length findings `shouldBe` 1

    it "returns empty list for unparseable input" $ do
      let output = "This is just text with no tab-separated sizes\n"
          findings = parseFindings 0 output
      findings `shouldBe` []

    it "parses various size suffixes" $ do
      let output = T.unlines
            [ "15K\t/small/file"
            , "2.5G\t/big/file"
            , "1T\t/huge/file"
            ]
          findings = parseFindings 0 output
      length findings `shouldBe` 3
      findingSize (findings !! 0) `shouldBe` round (15 * 1024 :: Double)
      findingSize (findings !! 1) `shouldBe` round (2.5 * 1024 * 1024 * 1024 :: Double)

    it "filters out findings below minimum size threshold" $ do
      let output = T.unlines
            [ "1.2G\t/home/user/.npm"
            , "500M\t/home/user/.cache/pip"
            , "15K\t/small/file"
            ]
          minBytes = 100 * 1024 * 1024  -- 100 MB
          findings = parseFindings minBytes output
      length findings `shouldBe` 2
      findingPath (head findings) `shouldBe` "/home/user/.npm"
      findingPath (findings !! 1) `shouldBe` "/home/user/.cache/pip"

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

  describe "validateAction" $ do
    it "warns when binary is not found" $ do
      let action = CleanupAction "test" "nonexistent_binary_xyz123 /tmp" "low" Nothing Nothing
      vr <- validateAction action
      validationOk vr `shouldBe` False
      validationWarnings vr `shouldSatisfy` any (\ w -> "Binary not found" `T.isInfixOf` w)

    it "warns when target path does not exist" $ do
      let action = CleanupAction "test" "rm /nonexistent/path/xyz123" "low" Nothing Nothing
      vr <- validateAction action
      validationWarnings vr `shouldSatisfy` any (\w -> "Path not found" `T.isInfixOf` w)

    it "passes for a valid command with existing binary" $ do
      let action = CleanupAction "test" "echo hello" "low" Nothing Nothing
      vr <- validateAction action
      validationWarnings vr `shouldSatisfy` all (\w -> not ("Binary not found" `T.isInfixOf` w))
