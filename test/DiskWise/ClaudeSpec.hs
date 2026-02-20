{-# LANGUAGE OverloadedStrings #-}

module DiskWise.ClaudeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Claude (buildPrompt, buildSystemPrompt, buildLearnPrompt,
                        buildGardenSystemPrompt, buildGardenPrompt,
                        parseAdvice, parseRefactorResult,
                        prefixCommitMsg, prefixGardenerMsg, extractJson)

spec :: Spec
spec = do
  describe "buildSystemPrompt" $ do
    it "includes JSON format instructions" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "cleanup_actions"
      prompt `shouldSatisfy` T.isInfixOf "wiki_contributions"
      prompt `shouldSatisfy` T.isInfixOf "analysis"

    it "includes risk level guidance" $ do
      buildSystemPrompt `shouldSatisfy` T.isInfixOf "risk_level"

    it "includes wiki page format template" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "## Where it stores data"
      prompt `shouldSatisfy` T.isInfixOf "## History"

    it "includes contribution guidance" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "diskwise-agent:"
      prompt `shouldSatisfy` T.isInfixOf "bar for contributing is LOW"

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
      buildPrompt "scan output" [] [] `shouldSatisfy` T.isInfixOf "No wiki pages available"

    it "includes novel findings section" $ do
      let finding = Finding "/tmp/big" 5000000000 "temp" "5GB temp file"
          prompt = buildPrompt "scan output" [] [finding]
      prompt `shouldSatisfy` T.isInfixOf "NOVEL FINDINGS"
      prompt `shouldSatisfy` T.isInfixOf "5GB temp file"

  describe "buildLearnPrompt" $ do
    it "includes session events" $ do
      let action = CleanupAction "Clean npm" "npm cache clean" "low" Nothing Nothing
          session = (emptySessionLog { logScanOutput = "scan" })
                      `addEvent` ActionExecuted action "cleaned 500MB"
                      `addEvent` ActionFailed action "permission denied"
          prompt = buildLearnPrompt session "agent@test"
      prompt `shouldSatisfy` T.isInfixOf "EXECUTED"
      prompt `shouldSatisfy` T.isInfixOf "cleaned 500MB"
      prompt `shouldSatisfy` T.isInfixOf "FAILED"
      prompt `shouldSatisfy` T.isInfixOf "permission denied"

    it "includes agent identity" $ do
      let prompt = buildLearnPrompt emptySessionLog "agent@myhost"
      prompt `shouldSatisfy` T.isInfixOf "agent@myhost"

  describe "buildGardenSystemPrompt" $ do
    it "mentions _meta/ notes" $ do
      let prompt = buildGardenSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "_meta/"
      prompt `shouldSatisfy` T.isInfixOf "notes to your future self"

    it "warns not to refactor _meta/ pages" $ do
      buildGardenSystemPrompt `shouldSatisfy` T.isInfixOf "Do NOT reorganize"

  describe "buildGardenPrompt" $ do
    it "separates content and meta sections" $ do
      let contentPage = WikiPage "tools/npm.md" "npm" "npm" "# npm content" "sha"
          metaPage = WikiPage "_meta/notes.md" "notes" "notes" "# gardener notes" "sha2"
          prompt = buildGardenPrompt [contentPage] [metaPage] "agent@x"
      prompt `shouldSatisfy` T.isInfixOf "WIKI CONTENT PAGES"
      prompt `shouldSatisfy` T.isInfixOf "# npm content"
      prompt `shouldSatisfy` T.isInfixOf "META PAGES"
      prompt `shouldSatisfy` T.isInfixOf "# gardener notes"

    it "shows first session message when no meta pages" $ do
      let contentPage = WikiPage "tools/npm.md" "npm" "npm" "# npm" "sha"
          prompt = buildGardenPrompt [contentPage] [] "agent@x"
      prompt `shouldSatisfy` T.isInfixOf "first session"

    it "includes agent identity" $ do
      let prompt = buildGardenPrompt [] [] "agent@myhost"
      prompt `shouldSatisfy` T.isInfixOf "agent@myhost"

  describe "prefixGardenerMsg" $ do
    it "adds gardener prefix" $
      prefixGardenerMsg "improve npm page" `shouldBe` "diskwise-gardener: improve npm page"

    it "does not double-prefix" $
      prefixGardenerMsg "diskwise-gardener: improve npm page"
        `shouldBe` "diskwise-gardener: improve npm page"

  describe "prefixCommitMsg" $ do
    it "adds prefix to plain messages" $
      prefixCommitMsg "add npm page" `shouldBe` "diskwise-agent: add npm page"

    it "does not double-prefix" $
      prefixCommitMsg "diskwise-agent: add npm page" `shouldBe` "diskwise-agent: add npm page"

  describe "extractJson" $ do
    it "extracts plain JSON" $ do
      extractJson "{\"key\": \"value\"}" `shouldBe` "{\"key\": \"value\"}"

    it "extracts JSON from markdown code blocks" $ do
      let wrapped = T.unlines
            [ "Here is the result:"
            , "```json"
            , "{\"key\": \"value\"}"
            , "```"
            ]
      extractJson wrapped `shouldBe` "{\"key\": \"value\"}"

    it "handles code blocks containing triple backticks in strings" $ do
      let wrapped = T.unlines
            [ "```json"
            , "{\"content\": \"# Title\\n```bash\\necho hi\\n```\\nDone.\"}"
            , "```"
            ]
      extractJson wrapped `shouldSatisfy` T.isInfixOf "echo hi"
      -- The extracted text should be valid JSON (starts with {, ends with })
      extractJson wrapped `shouldSatisfy` (\t -> T.head t == '{' && T.last t == '}')

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
      case parseAdvice "This is not JSON at all" of
        Left (ParseError _) -> pure ()
        _                   -> expectationFailure "Expected ParseError"

    it "handles missing optional fields gracefully" $ do
      case parseAdvice "{\"analysis\": \"Summary\"}" of
        Right advice -> do
          adviceAnalysis advice `shouldBe` "Summary"
          adviceCleanupActions advice `shouldBe` []
          adviceContributions advice `shouldBe` []
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseRefactorResult" $ do
    it "parses a complete refactoring response" $ do
      let json = T.unlines
            [ "{"
            , "  \"contributions\": ["
            , "    {"
            , "      \"type\": \"AmendPage\","
            , "      \"path\": \"tools/npm.md\","
            , "      \"content\": \"# npm (improved)\","
            , "      \"summary\": \"improve npm page\""
            , "    }"
            , "  ],"
            , "  \"done\": false,"
            , "  \"summary\": \"Improved npm page clarity\""
            , "}"
            ]
      case parseRefactorResult json of
        Right r -> do
          refactorDone r `shouldBe` False
          length (refactorContributions r) `shouldBe` 1
          refactorSummary r `shouldBe` "Improved npm page clarity"
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "defaults done=true when missing" $ do
      let json = "{\"contributions\": [], \"summary\": \"nothing to do\"}"
      case parseRefactorResult json of
        Right r -> refactorDone r `shouldBe` True
        Left err -> expectationFailure $ "Parse failed: " <> show err
