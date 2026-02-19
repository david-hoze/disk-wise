{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Claude
  ( investigate
  , proposeRefactoring
  , callClaude
  , callClaudeCode
  , callClaudeApi
  , buildPrompt
  , buildSystemPrompt
  , buildLearnPrompt
  , buildRefactorPrompt
  , parseAdvice
  , parseRefactorResult
  , agentIdentity
  , prefixCommitMsg
  ) where

import Control.Exception (catch, SomeException, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, proc)

import DiskWise.Types

-- | Generate an agent identity string for wiki History sections
-- Uses a hash of the machine's hostname for privacy
agentIdentity :: IO T.Text
agentIdentity = do
  result <- try $ readCreateProcessWithExitCode (proc "hostname" []) ""
    :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Right (ExitSuccess, out, _) ->
      let host = T.strip (T.pack out)
          short = T.take 8 host
      in pure ("agent@" <> short)
    _ -> pure "agent@unknown"

-- | Prefix a commit message with the diskwise-agent convention
prefixCommitMsg :: T.Text -> T.Text
prefixCommitMsg msg
  | "diskwise-agent:" `T.isPrefixOf` msg = msg
  | otherwise = "diskwise-agent: " <> msg

-- | Call Claude to investigate disk usage with wiki context
investigate :: AppConfig -> T.Text -> [(WikiPage, [Finding])] -> [Finding]
           -> IO (Either DiskWiseError ClaudeAdvice)
investigate config scanOutput matchedPages novelFindings = do
  let userPrompt = buildPrompt scanOutput matchedPages novelFindings
      sysPrompt = buildSystemPrompt
  result <- callClaude config sysPrompt userPrompt
  case result of
    Left err -> pure (Left err)
    Right text -> pure (parseAdvice text)

-- | Ask Claude to propose refactoring improvements to wiki pages
proposeRefactoring :: AppConfig -> [WikiPage] -> [FilePath] -> T.Text
                   -> IO (Either DiskWiseError RefactorResult)
proposeRefactoring config allPages touchedPaths identity = do
  let userPrompt = buildRefactorPrompt allPages touchedPaths identity
      sysPrompt = buildRefactorSystemPrompt
  result <- callClaude config sysPrompt userPrompt
  case result of
    Left err -> pure (Left err)
    Right text -> pure (parseRefactorResult text)

-- | Try Claude Code CLI first, fall back to API key
callClaude :: AppConfig -> T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callClaude config sysPrompt userPrompt = do
  cliResult <- callClaudeCode sysPrompt userPrompt
  case cliResult of
    Right text -> pure (Right text)
    Left _ ->
      if T.null (configApiKey config)
      then pure (Left (ClaudeError "No Claude access: CLI unavailable and no API key set"))
      else callClaudeApi config sysPrompt userPrompt

-- | Invoke Claude Code CLI as subprocess with --print flag
callClaudeCode :: T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callClaudeCode sysPrompt userPrompt = do
  let args = ["--print", "--system-prompt", T.unpack sysPrompt, T.unpack userPrompt]
  result <- try $ readCreateProcessWithExitCode (proc "claude" args) ""
  case result of
    Left (e :: SomeException) ->
      pure (Left (ClaudeError ("Claude CLI not available: " <> T.pack (show e))))
    Right (ExitSuccess, out, _) ->
      pure (Right (T.pack out))
    Right (ExitFailure code, _, err) ->
      pure (Left (ClaudeError ("Claude CLI failed (exit " <> T.pack (show code) <> "): " <> T.pack err)))

-- | Call Claude via the Anthropic API
callClaudeApi :: AppConfig -> T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callClaudeApi config sysPrompt userPrompt = do
  manager <- newManager tlsManagerSettings

  let reqBody = object
        [ "model"      .= configModel config
        , "max_tokens" .= (8192 :: Int)
        , "system"     .= sysPrompt
        , "messages"   .= [ object
            [ "role"    .= ("user" :: T.Text)
            , "content" .= userPrompt
            ] ]
        ]

  initialRequest <- parseRequest "https://api.anthropic.com/v1/messages"
  let request = initialRequest
        { method = "POST"
        , requestHeaders =
            [ (hContentType, "application/json")
            , ("x-api-key", TE.encodeUtf8 (configApiKey config))
            , ("anthropic-version", "2023-06-01")
            ]
        , requestBody = RequestBodyLBS (encode reqBody)
        }

  response <- httpLbs request manager
    `catch` (\(e :: SomeException) ->
      pure $ error ("API request failed: " <> show e))
  let body = responseBody response
      status = statusCode (responseStatus response)

  if status >= 200 && status < 300
    then case eitherDecode body of
      Left err ->
        pure (Left (ClaudeError ("Failed to parse API response: " <> T.pack err)))
      Right val ->
        pure (Right (extractTextContent val))
    else pure (Left (ClaudeError ("API returned status " <> T.pack (show status))))

-- | Extract text content from Claude API response JSON
extractTextContent :: Value -> T.Text
extractTextContent (Object obj) =
  case KM.lookup "content" obj of
    Just (Array blocks) ->
      T.unlines [ t | Object b <- foldMap pure blocks
                     , Just (String "text") <- [KM.lookup "type" b]
                     , Just (String t) <- [KM.lookup "text" b]
                ]
    _ -> "No content in response"
extractTextContent _ = "Unexpected response format"

-- | Build the system prompt defining Claude's role and output format
buildSystemPrompt :: T.Text
buildSystemPrompt = T.unlines
  [ "You are DiskWise, an AI disk cleanup advisor. You analyze filesystem scan data"
  , "and wiki knowledge to provide actionable cleanup advice."
  , ""
  , "You MUST respond with valid JSON in this exact structure:"
  , "{"
  , "  \"analysis\": \"Human-readable summary of what you found and recommend\","
  , "  \"cleanup_actions\": ["
  , "    {"
  , "      \"description\": \"What this cleanup does\","
  , "      \"command\": \"The shell command to run\","
  , "      \"risk_level\": \"low|medium|high\","
  , "      \"size_estimate\": \"~X.Y GB (optional, null if unknown)\","
  , "      \"wiki_ref\": \"path/to/wiki/page.md (optional, null if no wiki page)\""
  , "    }"
  , "  ],"
  , "  \"wiki_contributions\": ["
  , "    {"
  , "      \"type\": \"CreatePage|AmendPage\","
  , "      \"path\": \"category/page-name.md\","
  , "      \"content\": \"Full markdown content for the wiki page\","
  , "      \"summary\": \"Short description (will be prefixed with diskwise-agent:)\""
  , "    }"
  , "  ]"
  , "}"
  , ""
  , "Guidelines:"
  , "- Propose cleanup_actions for anything that can safely free space"
  , "- Set risk_level: low for caches/temp, medium for logs/old files, high for data"
  , "- Propose wiki_contributions when you discover ANYTHING useful that isn't in the wiki"
  , "- The bar for contributing is LOW. If it might save a future agent 5 seconds of"
  , "  reasoning or prevent a wrong suggestion, write it down."
  , "- Use CreatePage for new tools/topics, AmendPage to add info to existing pages"
  , "- Wiki pages should be generic (useful to anyone), not user-specific"
  , "- Replace absolute home paths with ~ in wiki content"
  , "- NEVER include usernames, secrets, tokens, or personal data in wiki content"
  , "- Do NOT re-document patterns already well-covered by existing wiki pages"
  , ""
  , "Wiki page format for tool pages:"
  , "  # Tool Name"
  , "  Brief description."
  , "  ## Where it stores data"
  , "  - `~/.some/path` — what this contains"
  , "  ## What's safe to delete"
  , "  ## What's NOT safe to delete"
  , "  ## Cleanup commands"
  , "  ## Typical space usage"
  , "  ## Platform notes"
  , "  ## History"
  , "  - YYYY-MM-DD: Description (AGENT_IDENTITY)"
  , ""
  , "Non-tool pages (troubleshooting, techniques, observations) are freeform but"
  , "MUST end with a ## History section for provenance tracking."
  , ""
  , "Things worth contributing:"
  , "- New tool/cache directories the wiki doesn't know about"
  , "- Safer cleanup methods than previously documented"
  , "- Caveats discovered the hard way"
  , "- Platform-specific paths"
  , "- Errors encountered during cleanup and how they were resolved"
  , "- Commands that silently failed and what to use instead"
  , "- Unexpected interactions between tools"
  , "- Typical sizes, growth rates, or patterns observed on real systems"
  ]

-- | Build the user message with scan output + wiki context + novel findings
buildPrompt :: T.Text -> [(WikiPage, [Finding])] -> [Finding] -> T.Text
buildPrompt scanOutput matchedPages novelFindings = T.unlines $
  [ "== SCAN OUTPUT =="
  , scanOutput
  , ""
  ] <>
  (if null matchedPages
   then ["== WIKI KNOWLEDGE ==", "(No wiki pages available)", ""]
   else [ "== WIKI KNOWLEDGE ==" ] <>
        concatMap formatMatchedPage matchedPages <>
        [""]
  ) <>
  (if null novelFindings
   then ["== NOVEL FINDINGS ==", "(No unmatched findings)", ""]
   else [ "== NOVEL FINDINGS (not covered by wiki) ==" ] <>
        map formatNovelFinding novelFindings <>
        ["", "Pay special attention to novel findings — consider creating wiki pages for them."]
  )
  where
    formatMatchedPage (page, findings) =
      [ "--- " <> pageTitle page <> " (" <> T.pack (pageRelPath page) <> ") ---"
      , pageBody page
      , "Matched findings: " <> T.pack (show (length findings))
      , ""
      ]
    formatNovelFinding f =
      "- " <> findingSummary f <> " [" <> findingCategory f <> "]"

-- | Build a learning prompt that includes the full session history
buildLearnPrompt :: SessionLog -> T.Text -> T.Text
buildLearnPrompt sessionLog identity = T.unlines $
  [ "== SESSION REVIEW =="
  , "Review this entire session and propose wiki contributions for anything useful learned."
  , ""
  , "== SCAN OUTPUT =="
  , logScanOutput sessionLog
  , ""
  , "== SESSION EVENTS =="
  ] <> map formatEvent (logEvents sessionLog) <>
  [ ""
  , "== AGENT IDENTITY =="
  , identity
  , ""
  , "Use this identity in History entries, e.g.:"
  , "- 2026-02-20: Description (" <> identity <> ")"
  ]
  where
    formatEvent (ActionExecuted action output) =
      "EXECUTED: " <> actionDescription action <> "\n  Output: " <> output
    formatEvent (ActionFailed action err) =
      "FAILED: " <> actionDescription action <> "\n  Error: " <> err
    formatEvent (ActionSkipped action) =
      "SKIPPED by user: " <> actionDescription action
    formatEvent (ContribPushed contrib) =
      "WIKI PUSHED: " <> T.pack (contribPath contrib)
    formatEvent (ContribFailed contrib err) =
      "WIKI FAILED: " <> T.pack (contribPath contrib) <> " — " <> err

-- | Build the refactoring system prompt
buildRefactorSystemPrompt :: T.Text
buildRefactorSystemPrompt = T.unlines
  [ "You are DiskWise's wiki refactoring agent. Your job is to improve the quality"
  , "and organization of the wiki after new content has been added."
  , ""
  , "You MUST respond with valid JSON in this exact structure:"
  , "{"
  , "  \"contributions\": ["
  , "    {"
  , "      \"type\": \"CreatePage|AmendPage\","
  , "      \"path\": \"category/page-name.md\","
  , "      \"content\": \"Full markdown content\","
  , "      \"summary\": \"What was improved\""
  , "    }"
  , "  ],"
  , "  \"done\": true/false,"
  , "  \"summary\": \"What was improved this pass, or why no improvements are needed\""
  , "}"
  , ""
  , "Set done=true if the wiki is well-organized and no meaningful improvements remain."
  , "Set done=false if you made changes and there might be more to improve."
  , ""
  , "Types of improvements:"
  , "- Merge duplicate or overlapping pages"
  , "- Rewrite unclear explanations"
  , "- Reorganize structure (move content to better locations)"
  , "- Split pages that got too long"
  , "- Create new category pages to tie related topics together"
  , "- Fix formatting inconsistencies"
  , "- Add missing ## History entries"
  , ""
  , "Do NOT:"
  , "- Remove useful information"
  , "- Add speculative content not based on the existing pages"
  , "- Change the meaning of existing advice"
  ]

-- | Build the user prompt for a refactoring pass
buildRefactorPrompt :: [WikiPage] -> [FilePath] -> T.Text -> T.Text
buildRefactorPrompt allPages touchedPaths identity = T.unlines $
  [ "== PAGES TOUCHED THIS SESSION =="
  ] <> map T.pack touchedPaths <>
  [ ""
  , "== ALL WIKI PAGES =="
  ] <> concatMap formatPage allPages <>
  [ ""
  , "== AGENT IDENTITY =="
  , identity
  , ""
  , "Review the pages touched this session and their surrounding pages."
  , "Propose improvements to organization, clarity, and structure."
  , "Each improvement should be a separate contribution."
  ]
  where
    formatPage page =
      [ "--- " <> T.pack (pageRelPath page) <> " ---"
      , pageBody page
      , ""
      ]

-- | Parse Claude's JSON response into ClaudeAdvice
parseAdvice :: T.Text -> Either DiskWiseError ClaudeAdvice
parseAdvice text =
  let jsonText = extractJson text
  in case eitherDecode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
    Left err -> Left (ParseError ("Failed to parse Claude's response as JSON: " <> T.pack err))
    Right val -> parseAdviceValue val

-- | Parse Claude's JSON response into RefactorResult
parseRefactorResult :: T.Text -> Either DiskWiseError RefactorResult
parseRefactorResult text =
  let jsonText = extractJson text
  in case eitherDecode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
    Left err -> Left (ParseError ("Failed to parse refactor response: " <> T.pack err))
    Right val -> parseRefactorValue val

-- | Extract JSON from text that might be wrapped in markdown code blocks
extractJson :: T.Text -> T.Text
extractJson text
  | "```json" `T.isInfixOf` text =
      let afterStart = T.drop 1 $ T.dropWhile (/= '\n') $ snd $ T.breakOn "```json" text
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | "```" `T.isInfixOf` text =
      let afterStart = T.drop 1 $ T.dropWhile (/= '\n') $ snd $ T.breakOn "```" text
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | otherwise = T.strip text

-- | Parse a JSON Value into ClaudeAdvice
parseAdviceValue :: Value -> Either DiskWiseError ClaudeAdvice
parseAdviceValue (Object obj) = do
  let analysis = case KM.lookup "analysis" obj of
        Just (String t) -> t
        _               -> "No analysis provided"

  let actions = case KM.lookup "cleanup_actions" obj of
        Just (Array arr) -> [a | Just a <- map parseCleanupAction (foldMap pure arr)]
        _                -> []

  let contribs = case KM.lookup "wiki_contributions" obj of
        Just (Array arr) -> [c | Just c <- map parseContribution (foldMap pure arr)]
        _                -> []

  Right ClaudeAdvice
    { adviceAnalysis       = analysis
    , adviceCleanupActions = actions
    , adviceContributions  = contribs
    }
parseAdviceValue _ = Left (ParseError "Expected JSON object from Claude")

-- | Parse a JSON Value into RefactorResult
parseRefactorValue :: Value -> Either DiskWiseError RefactorResult
parseRefactorValue (Object obj) = do
  let contribs = case KM.lookup "contributions" obj of
        Just (Array arr) -> [c | Just c <- map parseContribution (foldMap pure arr)]
        _                -> []

  let done = case KM.lookup "done" obj of
        Just (Bool b) -> b
        _             -> True  -- default to done if missing

  let summary = case KM.lookup "summary" obj of
        Just (String t) -> t
        _               -> ""

  Right RefactorResult
    { refactorContributions = contribs
    , refactorDone          = done
    , refactorSummary       = summary
    }
parseRefactorValue _ = Left (ParseError "Expected JSON object from Claude")

-- | Parse a cleanup action from Claude's snake_case JSON
parseCleanupAction :: Value -> Maybe CleanupAction
parseCleanupAction (Object o) = do
  let str key = case KM.lookup key o of
        Just (String t) -> Just t
        _               -> Nothing
      optStr key = case KM.lookup key o of
        Just (String t) -> Just (Just t)
        Just Null       -> Just Nothing
        Nothing         -> Just Nothing
        _               -> Nothing
  desc    <- str "description"
  cmd     <- str "command"
  risk    <- str "risk_level"
  sizeEst <- optStr "size_estimate"
  wikiRef <- optStr "wiki_ref"
  Just CleanupAction
    { actionDescription  = desc
    , actionCommand      = cmd
    , actionRiskLevel    = risk
    , actionSizeEstimate = sizeEst
    , actionWikiRef      = wikiRef
    }
parseCleanupAction _ = Nothing

-- | Parse a wiki contribution from Claude's snake_case JSON
parseContribution :: Value -> Maybe WikiContribution
parseContribution (Object o) = do
  let str key = case KM.lookup key o of
        Just (String t) -> Just t
        _               -> Nothing
  typStr  <- str "type"
  cPath   <- str "path"
  content <- str "content"
  summary <- str "summary"
  let cType = case typStr of
        "CreatePage" -> CreatePage
        "AmendPage"  -> AmendPage
        _            -> CreatePage
  Just WikiContribution
    { contribType    = cType
    , contribPath    = T.unpack cPath
    , contribContent = content
    , contribSummary = summary
    }
parseContribution _ = Nothing
