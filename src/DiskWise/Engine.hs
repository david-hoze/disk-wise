{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Engine
  ( investigate
  , callEngine
  , callEngineApi
  , callEngineGarden
  , buildPrompt
  , buildPromptWith
  , buildSystemPrompt
  , buildLearnPrompt
  , buildGardenSystemPrompt
  , buildGardenPrompt
  , parseAdvice
  , parseRefactorResult
  , agentIdentity
  , prefixCommitMsg
  , prefixGardenerMsg
  , extractJson
  , formatCommandStats
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

-- | Investigate disk usage with wiki context
investigate :: AppConfig -> T.Text -> [(WikiPage, [Finding])] -> [Finding]
           -> [CommandStats] -> [(FilePath, Integer)] -> [WikiPage] -> [WikiPage]
           -> Maybe [Integer] -> [(T.Text, Int)]
           -> IO (Either DiskWiseError AnalysisResult)
investigate config scanOutput matchedPages novelFindings cmdStats prevCleaned observationPages allPages diminishing zeroYield = do
  let userPrompt = buildPromptWith scanOutput matchedPages novelFindings cmdStats prevCleaned observationPages allPages diminishing zeroYield
      sysPrompt = buildSystemPrompt
  result <- callEngine config sysPrompt userPrompt
  case result of
    Left err -> pure (Left err)
    Right text -> pure (parseAdvice text)

-- | Prefix a commit message with the diskwise-gardener convention
prefixGardenerMsg :: T.Text -> T.Text
prefixGardenerMsg msg
  | "diskwise-gardener:" `T.isPrefixOf` msg = msg
  | otherwise = "diskwise-gardener: " <> msg

-- | Call the analysis engine API. Errors if no API key is set.
callEngine :: AppConfig -> T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callEngine config sysPrompt userPrompt =
  if T.null (configApiKey config)
  then pure (Left (EngineError "No API key set. Set DISKWISE_API_KEY or pass --api-key."))
  else callEngineApi config Nothing sysPrompt userPrompt

-- | Call the analysis engine with Opus 4.6 model override (for gardener)
callEngineGarden :: AppConfig -> T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callEngineGarden config sysPrompt userPrompt =
  if T.null (configApiKey config)
  then pure (Left (EngineError "No API key set. Set DISKWISE_API_KEY or pass --api-key."))
  else callEngineApi config (Just "engine-api") sysPrompt userPrompt

-- | Call the analysis engine via the API
callEngineApi :: AppConfig -> Maybe T.Text -> T.Text -> T.Text -> IO (Either DiskWiseError T.Text)
callEngineApi config modelOverride sysPrompt userPrompt = do
  manager <- newManager tlsManagerSettings

  let model = maybe (configModel config) id modelOverride
      reqBody = object
        [ "model"      .= model
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
        pure (Left (EngineError ("Failed to parse API response: " <> T.pack err)))
      Right val ->
        pure (Right (extractTextContent val))
    else pure (Left (EngineError ("API returned status " <> T.pack (show status))))

-- | Extract text content from API response JSON
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

-- | Build the system prompt defining the engine's role and output format
buildSystemPrompt :: T.Text
buildSystemPrompt = T.unlines
  [ "You are DiskWise, a disk cleanup advisor. You analyze filesystem scan data"
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
  , "- Wiki pages under CROSS-CUTTING WIKI RULES are HARD CONSTRAINTS."
  , "  Do NOT propose cleanup_actions that would remove items explicitly excluded there."
  , "- If a wiki page's \"What's NOT safe to delete\" section lists a path or pattern,"
  , "  do NOT suggest deleting it regardless of how much space it would free."
  , "- Do NOT propose cleanup_actions for paths listed under ZERO-YIELD PATHS."
  , "  These have been tried repeatedly and consistently free near-zero space."
  , "- cleanup_actions MUST ONLY contain commands that actually free disk space"
  , "  (rm, cache clean, prune, etc.). Do NOT include diagnostic commands"
  , "  (du, ls, df, find) as cleanup actions. Mention investigation suggestions"
  , "  in the analysis text instead."
  , "- When DIMINISHING RETURNS is active and the remaining opportunities require"
  , "  user decisions (e.g., which project's build artifacts to remove), you MAY"
  , "  include read-only investigation commands (du, find, ls) as cleanup_actions"
  , "  with risk_level \"investigation\". This surfaces actionable next steps the"
  , "  user can execute directly instead of burying them in analysis prose."
  , "- If the scan already shows a large reclaimable area, propose a direct"
  , "  cleanup command rather than an investigative command."
  , "- size_estimate MUST reflect the space freed by the exact command, not the"
  , "  total size of a parent directory. If the command targets a subdirectory,"
  , "  estimate only that subdirectory's size."
  , "- Set risk_level: low for caches/temp, medium for logs/old files, high for data"
  , "- Use CreatePage for new tools/topics, AmendPage to add info to existing pages"
  , "- Wiki pages should be generic (useful to anyone), not user-specific"
  , "- Replace absolute home paths with ~ in wiki content"
  , "- NEVER include usernames, secrets, tokens, or personal data in wiki content"
  , "- Do NOT re-document patterns already well-covered by existing wiki pages"
  , "- Before proposing a CreatePage contribution, check EXISTING WIKI PAGES."
  , "  If a page on the same tool or topic already exists, propose AmendPage instead."
  , "  Do NOT create a new page when an existing page covers the same subject,"
  , "  even if it has a different path or category prefix."
  , "- If a wiki page documents that a command fails on the current platform and"
  , "  provides an alternative command, propose the alternative as a cleanup action."
  , "  Do NOT omit the cleanup just because the original approach is unreliable."
  , "- When interpreting skip patterns: \"too risky\" and \"not applicable\" are clear"
  , "  signals. But \"not now\" (the default) is AMBIGUOUS — the user may want the"
  , "  cleanup but with more granularity, better targeting, or at a different time."
  , "  Do NOT treat repeated \"not now\" skips as permanent user preferences."
  , "  Instead, consider whether the action could be improved (e.g., showing sizes"
  , "  per subdirectory before bulk deletion)."
  , "- When multiple directories of the same type appear in scan output (e.g.,"
  , "  multiple dist-newstyle dirs), propose INDIVIDUAL cleanup actions for each"
  , "  rather than a single bulk find-exec command. This lets users selectively"
  , "  clean per-project. Include per-directory size estimates from the scan data."
  , ""
  , "When contributing to the wiki, focus on OBSERVATIONS FROM THIS SPECIFIC SYSTEM:"
  , "- Actual sizes you measured (not typical ranges from general knowledge)"
  , "- Commands that failed and why"
  , "- Platform-specific paths that differ from documentation"
  , "- Unexpected interactions between tools"
  , "- Corrections to existing wiki advice based on what you just saw"
  , ""
  , "Do NOT write generic tool documentation that is already widely known."
  , "If a tool page doesn't exist yet, create one, but populate it primarily"
  , "with concrete observations from this scan rather than general knowledge."
  , "Mark general knowledge sections with \"(general)\" and observed data with"
  , "\"(observed on this system, YYYY-MM-DD)\"."
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
  ]

-- | Build the user message with scan output + wiki context + novel findings
buildPrompt :: T.Text -> [(WikiPage, [Finding])] -> [Finding] -> T.Text
buildPrompt scanOutput matchedPages novelFindings = buildPromptWith scanOutput matchedPages novelFindings [] [] [] [] Nothing []

-- | Build prompt with optional command reliability stats, previously cleaned paths,
-- cross-cutting observation pages, full wiki page listing for duplicate prevention,
-- and diminishing returns data
buildPromptWith :: T.Text -> [(WikiPage, [Finding])] -> [Finding] -> [CommandStats]
               -> [(FilePath, Integer)] -> [WikiPage] -> [WikiPage] -> Maybe [Integer]
               -> [(T.Text, Int)] -> T.Text
buildPromptWith scanOutput matchedPages novelFindings cmdStats prevCleaned observationPages allPages diminishing zeroYield = T.unlines $
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
  ) <>
  (let statsText = formatCommandStats cmdStats
   in if T.null statsText then [] else [statsText]
  ) <>
  (if null prevCleaned then []
   else [ "== PREVIOUSLY CLEANED =="
        , "These paths were cleaned in a recent session. Avoid re-suggesting them"
        , "unless the scan shows they have significantly regrown."
        ] <> map formatPrevCleaned prevCleaned <> [""]
  ) <>
  (if null zeroYield then []
   else [ "== ZERO-YIELD PATHS (do not propose) =="
        , "These paths have been cleaned before but consistently freed < 1 MB:"
        ] <> map formatZeroYield zeroYield <>
        [ "Do NOT propose cleanup_actions targeting these paths."
        , ""
        ]
  ) <>
  (if null observationPages then []
   else [ "== CROSS-CUTTING WIKI RULES =="
        , "The following observation pages apply across all findings."
        , "Treat any prohibitions as HARD CONSTRAINTS — do NOT propose cleanup for excluded items."
        ] <> concatMap formatObservationPage observationPages <> [""]
  ) <>
  (if null allPages then []
   else [ "== EXISTING WIKI PAGES =="
        , "Before proposing a CreatePage, check this list. If a page on a similar topic"
        , "exists, use AmendPage to add to it instead."
        ] <> map formatPageListing allPages <> [""]
  ) <>
  (case diminishing of
    Just freed ->
      [ "== DIMINISHING RETURNS =="
      , "The last 3 sessions freed: " <> T.intercalate ", " (map formatMB freed) <> " — all under 10 MB."
      , "Cache-based cleanups have reached diminishing returns on this system."
      , "Instead of proposing cache cleanups that will free near-zero space:"
      , "- Focus on structural observations (large directories the user could reorganize)"
      , "- Mention in the analysis that safe automated cleanup has been exhausted"
      , "- Only propose cleanup_actions if they would free meaningful space (> 50 MB)"
      , ""
      ]
    Nothing -> []
  )
  where
    formatMB b = T.pack (show (b `div` (1024 * 1024))) <> " MB"
    formatPageListing page =
      "- " <> T.pack (pageRelPath page) <> " — " <> pageTitle page
    formatObservationPage page =
      [ "--- " <> T.pack (pageRelPath page) <> " ---"
      , pageBody page
      ]
    formatZeroYield (path, count) =
      "- " <> path <> " (cleaned " <> T.pack (show count) <> " times, avg 0 MB)"
    formatPrevCleaned (path, bytes) =
      "- " <> T.pack path <> " (freed " <> formatBytes bytes <> ")"
    formatMatchedPage (page, findings) =
      [ "--- " <> pageTitle page <> " (" <> T.pack (pageRelPath page) <> ") ---"
      , pageBody page
      , "Matched findings: " <> T.pack (show (length findings))
      ] <> formatOutcomeHistory page <>
      [ "" ]
    formatNovelFinding f =
      "- " <> findingSummary f <> " [" <> findingCategory f <> "]"

-- | Format outcome history for a wiki page (verify/fail counts)
formatOutcomeHistory :: WikiPage -> [T.Text]
formatOutcomeHistory page
  | pageVerifyCount page == 0 && pageFailCount page == 0 = []
  | otherwise =
      [ "Outcome history: " <> T.pack (show (pageVerifyCount page)) <> " verified, "
        <> T.pack (show (pageFailCount page)) <> " failed" ]

-- | Build a learning prompt that includes the full session history
buildLearnPrompt :: SessionLog -> T.Text -> T.Text -> [WikiPage] -> Maybe [Integer] -> T.Text
buildLearnPrompt sessionLog identity historyContext allPages diminishing =
  let plat = logPlatform sessionLog
  in T.unlines $
  [ "== SESSION REVIEW =="
  , "Review this entire session and propose wiki contributions for anything useful learned."
  , ""
  , "Before proposing a new page, check if any existing page already covers this tool."
  , "If so, propose an AmendPage with your new observations rather than a CreatePage."
  , ""
  , "== PLATFORM =="
  , "OS: " <> platformOS plat <> " | Arch: " <> platformArch plat
    <> " | Shell: " <> platformShell plat
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
  , ""
  , "If the actual space freed differs significantly from the wiki's estimate,"
  , "consider amending the wiki page with an observation about real-world sizes."
  , ""
  , "When documenting failures or platform-specific behavior, include the platform"
  , "in the wiki page's ## Platform notes section. Use the format:"
  , "\"On [OS] ([arch]): [observation]\""
  , ""
  ] <> (if T.null historyContext then [] else [historyContext]) <>
  (if null allPages then []
   else [ "== EXISTING WIKI PAGES =="
        , "Before proposing a CreatePage, check this list. If a page on a similar topic"
        , "exists, use AmendPage to add to it instead."
        ] <> map formatLearnPageListing allPages <> [""]
  ) <>
  (case diminishing of
    Just freed ->
      [ "== DIMINISHING RETURNS =="
      , "The last 3 sessions freed: " <> T.intercalate ", "
          (map (\b -> T.pack (show (b `div` (1024 * 1024))) <> " MB") freed)
        <> " — all under 10 MB."
      , "Cache-based cleanups have reached diminishing returns on this system."
      , "Document this pattern in relevant wiki pages if not already noted."
      , ""
      ]
    Nothing -> []
  ) <>
  [ "USER FEEDBACK IS THE HIGHEST-VALUE SIGNAL. If the user reported a problem:"
  , "1. Identify which cleanup action likely caused it."
  , "2. Amend the relevant wiki page's \"What's NOT safe to delete\" section."
  , "3. If the action came from a wiki recommendation, add a warning."
  , "This is more important than any other wiki contribution this session."
  ]
  where
    formatEvent (ActionExecuted outcome) =
      "EXECUTED (proposed #" <> T.pack (show (outcomePosition outcome + 1))
      <> ", executed #" <> T.pack (show (outcomeOrder outcome + 1))
      <> "): " <> actionDescription (outcomeAction outcome)
      <> "\n  Output: " <> outcomeMessage outcome
      <> "\n  Expected: " <> maybe "(unknown)" id (outcomeExpected outcome)
      <> " | Actual freed: " <> maybe "(not measured)" formatBytes (outcomeBytesFreed outcome)
    formatEvent (ActionFailed outcome) =
      "FAILED (proposed #" <> T.pack (show (outcomePosition outcome + 1))
      <> "): " <> actionDescription (outcomeAction outcome)
      <> "\n  Error: " <> outcomeMessage outcome
    formatEvent (ActionSkipped action reason) =
      "SKIPPED by user: " <> actionDescription action
      <> " (reason: " <> formatSkipReason reason <> ")"
    formatEvent (ContribPushed contrib decision) =
      case decision of
        ContribApproved ->
          "WIKI PUSHED: " <> T.pack (contribPath contrib) <> " (user approved without edits)"
        ContribEdited note ->
          "WIKI PUSHED: " <> T.pack (contribPath contrib) <> " (user edited: " <> note <> ")"
        ContribSkipped ->
          "WIKI SKIPPED: " <> T.pack (contribPath contrib)
    formatEvent (ContribFailed contrib err) =
      "WIKI FAILED: " <> T.pack (contribPath contrib) <> " — " <> err
    formatEvent (UserFeedback feedback) =
      "USER FEEDBACK: \"" <> feedback <> "\""
    formatLearnPageListing page =
      "- " <> T.pack (pageRelPath page) <> " — " <> pageTitle page

-- | Format a byte count as a human-readable string
formatBytes :: Integer -> T.Text
formatBytes b
  | b >= 1024 * 1024 * 1024 = T.pack (show (b `div` (1024 * 1024 * 1024))) <> " GB"
  | b >= 1024 * 1024        = T.pack (show (b `div` (1024 * 1024))) <> " MB"
  | b >= 1024               = T.pack (show (b `div` 1024)) <> " KB"
  | otherwise               = T.pack (show b) <> " B"

-- | Format a skip reason for display in prompts
formatSkipReason :: SkipReason -> T.Text
formatSkipReason TooRisky            = "too risky"
formatSkipReason NotNow              = "not now"
formatSkipReason AlreadyHandled      = "already handled"
formatSkipReason NotApplicable       = "not applicable"
formatSkipReason (SkipReasonOther t) = t

-- | Format command reliability statistics for inclusion in prompts
formatCommandStats :: [CommandStats] -> T.Text
formatCommandStats [] = ""
formatCommandStats stats = T.unlines $
  [ "== COMMAND RELIABILITY =="
  , "Cross-session statistics for cleanup commands:"
  ] <> map formatOne stats <>
  [ ""
  , "If a command fails frequently, consider:"
  , "- Adding platform-specific notes to its wiki page"
  , "- Suggesting an alternative command"
  , "- Raising the risk level if it's unreliable"
  ]
  where
    formatOne cs =
      let total = cmdStatsSuccesses cs + cmdStatsFailures cs
          rate = if total > 0
                 then (cmdStatsSuccesses cs * 100) `div` total
                 else 0
      in "- `" <> cmdStatsCommand cs <> "`: "
         <> T.pack (show (cmdStatsSuccesses cs)) <> "/" <> T.pack (show total)
         <> " succeeded (" <> T.pack (show rate) <> "% reliable)"
         <> maybe "" (\e -> "\n  Last error: " <> e) (cmdStatsLastError cs)

-- | Build the gardener-specific system prompt
buildGardenSystemPrompt :: T.Text
buildGardenSystemPrompt = T.unlines
  [ "You are DiskWise's wiki gardener. Your job is to improve the quality"
  , "and organization of the wiki knowledge base."
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
  , "IMPORTANT about _meta/ pages:"
  , "_meta/ pages are YOUR notes to your future self, not content to refactor."
  , "Do NOT reorganize, merge, or rewrite _meta/ pages as if they were wiki content."
  , "You may CREATE or AMEND _meta/ pages to record your own gardening notes."
  , ""
  , "Do NOT:"
  , "- Remove useful information"
  , "- Add speculative content not based on the existing pages"
  , "- Change the meaning of existing advice"
  , "- Treat _meta/ pages as wiki content to be refactored"
  ]

-- | Build the user prompt for a gardening pass
buildGardenPrompt :: [WikiPage] -> [WikiPage] -> T.Text -> T.Text
buildGardenPrompt contentPages metaPages identity = T.unlines $
  [ "== WIKI CONTENT PAGES =="
  ] <> concatMap formatPage contentPages <>
  [ ""
  , "== META PAGES (your notes from previous sessions) =="
  ] <>
  (if null metaPages
   then ["(No meta pages yet — this is your first session.)"]
   else concatMap formatPage metaPages
  ) <>
  [ ""
  , "== AGENT IDENTITY =="
  , identity
  , ""
  , "Review the wiki content pages and improve their organization, clarity, and structure."
  , "Each improvement should be a separate contribution."
  , "You may also write to _meta/ pages to leave notes for your next gardening session."
  ]
  where
    formatPage page =
      [ "--- " <> T.pack (pageRelPath page) <> " ---"
      , pageBody page
      ] <> formatOutcomeHistory page <>
      [ "" ]

-- | Parse JSON response into AnalysisResult
parseAdvice :: T.Text -> Either DiskWiseError AnalysisResult
parseAdvice text =
  let jsonText = extractJson text
  in case eitherDecode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
    Left err -> Left (ParseError ("Failed to parse response as JSON: " <> T.pack err))
    Right val -> parseAdviceValue val

-- | Parse JSON response into RefactorResult
parseRefactorResult :: T.Text -> Either DiskWiseError RefactorResult
parseRefactorResult text =
  let jsonText = extractJson text
  in case eitherDecode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
    Left err -> Left (ParseError ("Failed to parse refactor response: " <> T.pack err))
    Right val -> parseRefactorValue val

-- | Extract JSON from text that might be wrapped in markdown code blocks.
-- The response is always a JSON object, so we extract from the first { to
-- the last }. This avoids issues with ``` appearing inside JSON strings
-- (e.g., wiki contributions containing markdown code blocks).
extractJson :: T.Text -> T.Text
extractJson text =
  let afterOpen = T.dropWhile (/= '{') text
      beforeClose = T.dropWhileEnd (/= '}') afterOpen
  in if T.null afterOpen || T.null beforeClose
     then T.strip text
     else beforeClose

-- | Parse a JSON Value into an AnalysisResult
parseAdviceValue :: Value -> Either DiskWiseError AnalysisResult
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

  Right AnalysisResult
    { adviceAnalysis       = analysis
    , adviceCleanupActions = actions
    , adviceContributions  = contribs
    }
parseAdviceValue _ = Left (ParseError "Expected JSON object in response")

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
parseRefactorValue _ = Left (ParseError "Expected JSON object in response")

-- | Parse a cleanup action from snake_case JSON
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

-- | Parse a wiki contribution from snake_case JSON
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
