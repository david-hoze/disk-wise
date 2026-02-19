{-# LANGUAGE OverloadedStrings #-}

module DiskWise.ClaudeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Claude (buildPrompt, buildSystemPrompt, parseAdvice)

spec :: Spec
spec = do
  describe "buildSystemPrompt" $ do
    it "includes JSON format instructions" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "cleanup_actions"
      prompt `shouldSatisfy` T.isInfixOf "wiki_contributions"
      prompt `shouldSatisfy` T.isInfixOf "analysis"

    it "includes risk level guidance" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "risk_level"

  describe "buildPrompt" $ do
    it "includes scan output" $ do
      let prompt = buildPrompt "scan data here" [] []
      prompt `shouldSatisfy` T.isInfixOf "scan data here"

    it "shows wiki pages when available" $ do
      let page = WikiPage "tools/npm.md" "npm" "npm cleanup"
                   "# npm\nClean with npm cache clean" "sha123"
          finding = Finding "/home/.npm" 1000000 "cache" "npm cache 1GB"
          prompt = buildPrompt "scan output" [(page, [finding])] []
      prompt `shouldSatisfy` T.isInfixOf "npm cleanup"
      prompt `shouldSatisfy` T.isInfixOf "tools/npm.md"

    it "shows placeholder when no wiki pages" $ do
      let prompt = buildPrompt "scan output" [] []
      prompt `shouldSatisfy` T.isInfixOf "No wiki pages available"

    it "includes novel findings section" $ do
      let finding = Finding "/tmp/big" 5000000000 "temp" "5GB temp file"
          prompt = buildPrompt "scan output" [] [finding]
      prompt `shouldSatisfy` T.isInfixOf "NOVEL FINDINGS"
      prompt `shouldSatisfy` T.isInfixOf "5GB temp file"

  describe "parseAdvice" $ do
    it "parses valid JSON response" $ do
      let json = T.unlines
            [ "{"
            , "  \"analysis\": \"Found 5GB of caches.\","
            , "  \"cleanup_actions\": ["
            , "    {"
            , "      \"description\": \"Clean npm cache\","
            , "      \"command\": \"npm cache clean --force\","
            , "      \"risk_level\": \"low\","
            , "      \"size_estimate\": \"~2 GB\","
            , "      \"wiki_ref\": \"tools/npm.md\""
            , "    }"
            , "  ],"
            , "  \"wiki_contributions\": ["
            , "    {"
            , "      \"type\": \"CreatePage\","
            , "      \"path\": \"tools/yarn.md\","
            , "      \"content\": \"# Yarn\\nClean with yarn cache clean\","
            , "      \"summary\": \"Add yarn cleanup page\""
            , "    }"
            , "  ]"
            , "}"
            ]
      case parseAdvice json of
        Right advice -> do
          adviceAnalysis advice `shouldBe` "Found 5GB of caches."
          length (adviceCleanupActions advice) `shouldBe` 1
          actionCommand (head (adviceCleanupActions advice)) `shouldBe` "npm cache clean --force"
          length (adviceContributions advice) `shouldBe` 1
          contribPath (head (adviceContributions advice)) `shouldBe` "tools/yarn.md"
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses JSON wrapped in markdown code blocks" $ do
      let json = T.unlines
            [ "Here is my analysis:"
            , "```json"
            , "{"
            , "  \"analysis\": \"All clean.\","
            , "  \"cleanup_actions\": [],"
            , "  \"wiki_contributions\": []"
            , "}"
            , "```"
            ]
      case parseAdvice json of
        Right advice -> adviceAnalysis advice `shouldBe` "All clean."
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "returns error for invalid JSON" $ do
      let json = "This is not JSON at all"
      case parseAdvice json of
        Left (ParseError _) -> pure ()
        _                   -> expectationFailure "Expected ParseError"

    it "handles missing optional fields gracefully" $ do
      let json = "{\"analysis\": \"Summary\"}"
      case parseAdvice json of
        Right advice -> do
          adviceAnalysis advice `shouldBe` "Summary"
          adviceCleanupActions advice `shouldBe` []
          adviceContributions advice `shouldBe` []
        Left err -> expectationFailure $ "Parse failed: " <> show err
