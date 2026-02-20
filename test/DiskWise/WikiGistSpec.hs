{-# LANGUAGE OverloadedStrings #-}

module DiskWise.WikiGistSpec (spec) where

import Test.Hspec

import DiskWise.WikiGist (encodeGistPath, decodeGistPath)

spec :: Spec
spec = do
  describe "encodeGistPath" $ do
    it "encodes a simple path with one directory" $ do
      encodeGistPath "tools/npm.md" `shouldBe` "tools--npm.md"

    it "encodes a nested path" $ do
      encodeGistPath "os/macos/xcode.md" `shouldBe` "os--macos--xcode.md"

    it "encodes a _meta/ path" $ do
      encodeGistPath "_meta/gardening-log.md" `shouldBe` "_meta--gardening-log.md"

    it "leaves a flat filename unchanged" $ do
      encodeGistPath "readme.md" `shouldBe` "readme.md"

  describe "decodeGistPath" $ do
    it "decodes a simple encoded path" $ do
      decodeGistPath "tools--npm.md" `shouldBe` "tools/npm.md"

    it "decodes a nested encoded path" $ do
      decodeGistPath "os--macos--xcode.md" `shouldBe` "os/macos/xcode.md"

    it "decodes a _meta/ encoded path" $ do
      decodeGistPath "_meta--gardening-log.md" `shouldBe` "_meta/gardening-log.md"

    it "leaves a flat filename unchanged" $ do
      decodeGistPath "readme.md" `shouldBe` "readme.md"

  describe "roundtrip" $ do
    it "encode then decode is identity for directory paths" $ do
      let paths = [ "tools/npm.md"
                  , "os/macos/xcode.md"
                  , "_meta/gardening-log.md"
                  , "haskell/cabal.md"
                  ]
      mapM_ (\p -> decodeGistPath (encodeGistPath p) `shouldBe` p) paths

    it "encode then decode is identity for flat paths" $ do
      decodeGistPath (encodeGistPath "readme.md") `shouldBe` "readme.md"
