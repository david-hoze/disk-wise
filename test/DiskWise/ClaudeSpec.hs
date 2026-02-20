{-# LANGUAGE OverloadedStrings #-}

module DiskWise.ClaudeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import DiskWise.Types
import DiskWise.Claude (buildPrompt, buildPromptWith, buildSystemPrompt,
                        buildLearnPrompt,
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
      prompt `shouldSatisfy` T.isInfixOf "OBSERVATIONS FROM THIS SPECIFIC SYSTEM"
      prompt `shouldSatisfy` T.isInfixOf "Do NOT write generic tool documentation"

    it "includes diagnostic command guidance" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "diagnostic"
      prompt `shouldSatisfy` T.isInfixOf "Do NOT include diagnostic commands"

    it "includes HARD CONSTRAINT language for wiki rules" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "HARD CONSTRAINT"
      prompt `shouldSatisfy` T.isInfixOf "Do NOT propose cleanup_actions that would remove items explicitly excluded"

    it "includes exact command scope guidance for size estimates" $ do
      let prompt = buildSystemPrompt
      prompt `shouldSatisfy` T.isInfixOf "exact command"
      prompt `shouldSatisfy` T.isInfixOf "size_estimate MUST reflect the space freed"

  describe "buildPrompt" $ do
    it "includes scan output" $ do
      let prompt = buildPrompt "scan data here" [] []
      prompt `shouldSatisfy` T.isInfixOf "scan data here"

    it "shows wiki pages when available" $ do
      let page = WikiPage "tools/npm.md" "npm" "npm cleanup"
                   "# npm\nClean with npm cache clean" "sha123" Nothing 0 0
          finding = Finding "/home/.npm" 1000000 "cache" "npm cache 1GB"
          prompt = buildPrompt "scan output" [(page, [finding])] []
      prompt `shouldSatisfy` T.isInfixOf "npm cleanup"
      prompt `shouldSatisfy` T.isInfixOf "tools/npm.md"

    it "shows placeholder when no wiki pages" $ do
      buildPrompt "scan output" [] [] `shouldSatisfy` T.isInfixOf "No wiki pages available"

    it "includes outcome history when page has verify/fail counts" $ do
      let page = WikiPage "tools/npm.md" "npm" "npm"
                   "# npm" "sha" Nothing 5 2
          finding = Finding "/home/.npm" 1000000 "cache" "npm cache"
          prompt = buildPrompt "scan output" [(page, [finding])] []
      prompt `shouldSatisfy` T.isInfixOf "5 verified"
      prompt `shouldSatisfy` T.isInfixOf "2 failed"

    it "omits outcome history when counts are zero" $ do
      let page = WikiPage "tools/npm.md" "npm" "npm"
                   "# npm" "sha" Nothing 0 0
          finding = Finding "/home/.npm" 1000000 "cache" "npm cache"
          prompt = buildPrompt "scan output" [(page, [finding])] []
      prompt `shouldSatisfy` (not . T.isInfixOf "verified")

    it "includes novel findings section" $ do
      let finding = Finding "/tmp/big" 5000000000 "temp" "5GB temp file"
          prompt = buildPrompt "scan output" [] [finding]
      prompt `shouldSatisfy` T.isInfixOf "NOVEL FINDINGS"
      prompt `shouldSatisfy` T.isInfixOf "5GB temp file"

  describe "buildPromptWith previously cleaned paths" $ do
    it "includes PREVIOUSLY CLEANED section when paths are non-empty" $ do
      let prompt = buildPromptWith "scan output" [] [] []
                     [("/home/.npm", 524288000), ("/home/.cache", 1073741824)] []
      prompt `shouldSatisfy` T.isInfixOf "PREVIOUSLY CLEANED"
      prompt `shouldSatisfy` T.isInfixOf "/home/.npm"
      prompt `shouldSatisfy` T.isInfixOf "/home/.cache"

    it "omits PREVIOUSLY CLEANED section when paths are empty" $ do
      let prompt = buildPromptWith "scan output" [] [] [] [] []
      prompt `shouldSatisfy` (not . T.isInfixOf "PREVIOUSLY CLEANED")

  describe "buildPromptWith observation pages" $ do
    it "includes CROSS-CUTTING WIKI RULES when observation pages are non-empty" $ do
      let obsPage = WikiPage "observations/skip-patterns-windows.md" "skip-patterns"
                      "Skip Patterns" "Do NOT delete .blend files" "sha" Nothing 0 0
          prompt = buildPromptWith "scan output" [] [] [] [] [obsPage]
      prompt `shouldSatisfy` T.isInfixOf "CROSS-CUTTING WIKI RULES"
      prompt `shouldSatisfy` T.isInfixOf "HARD CONSTRAINTS"
      prompt `shouldSatisfy` T.isInfixOf "Do NOT delete .blend files"
      prompt `shouldSatisfy` T.isInfixOf "observations/skip-patterns-windows.md"

    it "omits CROSS-CUTTING WIKI RULES when observation pages are empty" $ do
      let prompt = buildPromptWith "scan output" [] [] [] [] []
      prompt `shouldSatisfy` (not . T.isInfixOf "CROSS-CUTTING WIKI RULES")

  describe "buildLearnPrompt" $ do
    it "includes session events" $ do
      let action = CleanupAction "Clean npm" "npm cache clean" "low" Nothing Nothing
          okOutcome = CleanupOutcome action True "cleaned 500MB" (Just 524288000) Nothing 0 0
          failOutcome = CleanupOutcome action False "permission denied" Nothing Nothing 1 1
          session = (emptySessionLog { logScanOutput = "scan" })
                      `addEvent` ActionExecuted okOutcome
                      `addEvent` ActionFailed failOutcome
          prompt = buildLearnPrompt session "agent@test" ""
      prompt `shouldSatisfy` T.isInfixOf "EXECUTED"
      prompt `shouldSatisfy` T.isInfixOf "cleaned 500MB"
      prompt `shouldSatisfy` T.isInfixOf "FAILED"
      prompt `shouldSatisfy` T.isInfixOf "permission denied"

    it "includes agent identity" $ do
      let prompt = buildLearnPrompt emptySessionLog "agent@myhost" ""
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
      let contentPage = WikiPage "tools/npm.md" "npm" "npm" "# npm content" "sha" Nothing 0 0
          metaPage = WikiPage "_meta/notes.md" "notes" "notes" "# gardener notes" "sha2" Nothing 0 0
          prompt = buildGardenPrompt [contentPage] [metaPage] "agent@x"
      prompt `shouldSatisfy` T.isInfixOf "WIKI CONTENT PAGES"
      prompt `shouldSatisfy` T.isInfixOf "# npm content"
      prompt `shouldSatisfy` T.isInfixOf "META PAGES"
      prompt `shouldSatisfy` T.isInfixOf "# gardener notes"

    it "includes outcome history in garden prompt" $ do
      let page = WikiPage "tools/npm.md" "npm" "npm" "# npm" "sha" Nothing 3 1
          prompt = buildGardenPrompt [page] [] "agent@x"
      prompt `shouldSatisfy` T.isInfixOf "3 verified"
      prompt `shouldSatisfy` T.isInfixOf "1 failed"

    it "shows first session message when no meta pages" $ do
      let contentPage = WikiPage "tools/npm.md" "npm" "npm" "# npm" "sha" Nothing 0 0
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
