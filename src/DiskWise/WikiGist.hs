{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.WikiGist
  ( fetchTree
  , fetchFullTree
  , fetchPage
  , createPage
  , updatePage
  , pushContribution
  , recordOutcome
  , encodeGistPath
  , decodeGistPath
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType, hAuthorization)
import Network.HTTP.Types.Status (statusCode)

import DiskWise.Types
import DiskWise.Wiki (parseMetaComment, renderMetaComment, updatePageMeta,
                      stripMetaLines, PageMeta(..), defaultPageMeta)

-- | Encode a wiki path for use as a gist filename.
-- Replaces @/@ with @--@ since gists are flat (no directories).
-- e.g. @"tools/npm.md"@ → @"tools--npm.md"@
encodeGistPath :: FilePath -> T.Text
encodeGistPath = T.replace "/" "--" . T.pack

-- | Decode a gist filename back to a wiki path.
-- Replaces @--@ with @/@.
-- e.g. @"tools--npm.md"@ → @"tools/npm.md"@
decodeGistPath :: T.Text -> FilePath
decodeGistPath = T.unpack . T.replace "--" "/"

-- | Fetch the tree of wiki pages from a GitHub Gist (excludes _meta/ pages).
fetchTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchTree config = do
  result <- gistGet config
  case result of
    Left err -> pure (Left err)
    Right val -> pure (Right (parseGistPages False val))

-- | Fetch the full tree including _meta/ pages.
fetchFullTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchFullTree config = do
  result <- gistGet config
  case result of
    Left err -> pure (Left err)
    Right val -> pure (Right (parseGistPages True val))

-- | Fetch a single page from the gist by its wiki path.
fetchPage :: AppConfig -> FilePath -> IO (Either DiskWiseError WikiPage)
fetchPage config path = do
  result <- gistGet config
  case result of
    Left err -> pure (Left err)
    Right val ->
      let encoded = encodeGistPath path
      in case findGistFile encoded val of
        Just page -> pure (Right page)
        Nothing -> pure (Left (GitHubApiError ("Page not found in gist: " <> T.pack path)))

-- | Create a new page in the gist.
createPage :: AppConfig -> WikiContribution -> IO (Either DiskWiseError ())
createPage config contrib = do
  let filename = encodeGistPath (contribPath contrib)
      withMeta = renderMetaComment defaultPageMeta <> stripMetaLines (contribContent contrib)
      body = object
        [ "files" .= object
            [ Key.fromText filename .= object
                [ "content" .= withMeta ]
            ]
        ]
  result <- gistPatch config body
  case result of
    Left err -> pure (Left err)
    Right _  -> pure (Right ())

-- | Update an existing page in the gist.
-- Gists have no SHA-based concurrency — last write wins.
updatePage :: AppConfig -> WikiPage -> WikiContribution -> IO (Either DiskWiseError ())
updatePage config page contrib = do
  let filename = encodeGistPath (contribPath contrib)
      pageMeta = PageMeta
        { metaLastVerified = pageLastVerified page
        , metaVerifyCount  = pageVerifyCount page
        , metaFailCount    = pageFailCount page
        }
      withMeta = renderMetaComment pageMeta <> stripMetaLines (contribContent contrib)
      body = object
        [ "files" .= object
            [ Key.fromText filename .= object
                [ "content" .= withMeta ]
            ]
        ]
  result <- gistPatch config body
  case result of
    Left err -> pure (Left err)
    Right _  -> pure (Right ())

-- | Push a contribution (create or amend).
-- Gists don't have SHA conflicts, so no retry logic needed.
pushContribution :: AppConfig -> [WikiPage] -> WikiContribution -> IO (Either DiskWiseError ())
pushContribution config pages contrib =
  case contribType contrib of
    CreatePage -> createPage config contrib
    AmendPage  ->
      case filter (\p -> pageRelPath p == contribPath contrib) pages of
        (page:_) -> updatePage config page contrib
        []       -> createPage config contrib

-- | Record a cleanup outcome against a wiki page's metadata.
recordOutcome :: AppConfig -> [WikiPage] -> FilePath -> Bool -> IO ()
recordOutcome config pages wikiRef success = do
  case filter (\p -> pageRelPath p == wikiRef) pages of
    (page:_) -> do
      updater <- updatePageMeta success
      let updated = updater page
          meta = PageMeta
            { metaLastVerified = pageLastVerified updated
            , metaVerifyCount  = pageVerifyCount updated
            , metaFailCount    = pageFailCount updated
            }
          newContent = renderMetaComment meta <> pageBody updated
          contrib = WikiContribution
            { contribType    = AmendPage
            , contribPath    = pageRelPath page
            , contribContent = newContent
            , contribSummary = "diskwise-agent: update outcome metadata"
            }
      _ <- pushContribution config pages contrib
      pure ()
    [] -> pure ()
  `catch` (\(_ :: SomeException) -> pure ())

-- | Parse all files from a gist response into WikiPages.
parseGistPages :: Bool -> Value -> [WikiPage]
parseGistPages includeMeta (Object obj) =
  case KM.lookup "files" obj of
    Just (Object files) ->
      [ page
      | (k, v) <- KM.toList files
      , let filename = Key.toText k
            path = decodeGistPath filename
      , T.isSuffixOf ".md" filename
      , filename /= "Home.md"
      , includeMeta || not (T.isPrefixOf "_meta--" filename)
      , Just page <- [parseGistFile path v]
      ]
    _ -> []
parseGistPages _ _ = []

-- | Find a specific file in a gist response by its encoded filename.
findGistFile :: T.Text -> Value -> Maybe WikiPage
findGistFile filename (Object obj) =
  case KM.lookup "files" obj of
    Just (Object files) ->
      case KM.lookup (Key.fromText filename) files of
        Just v -> parseGistFile (decodeGistPath filename) v
        Nothing -> Nothing
    _ -> Nothing
findGistFile _ _ = Nothing

-- | Parse a single gist file object into a WikiPage.
parseGistFile :: FilePath -> Value -> Maybe WikiPage
parseGistFile path (Object obj) = do
  content <- case KM.lookup "content" obj of
    Just (String c) -> Just c
    _ -> Nothing
  let (meta, body) = parseMetaComment content
      topic = T.toLower $ T.pack $ takeWhile (/= '.') $ last $ splitPath path
      title = extractTitle body
  Just WikiPage
    { pageRelPath      = path
    , pageTopic        = topic
    , pageTitle        = title
    , pageBody         = body
    , pageSha          = ""  -- Gists don't use SHA-based updates
    , pageLastVerified = metaLastVerified meta
    , pageVerifyCount  = metaVerifyCount meta
    , pageFailCount    = metaFailCount meta
    }
  where
    splitPath = go []
    go acc [] = reverse (reverse [] : acc)
    go acc (c:cs)
      | c == '/' = go (reverse [] : acc) cs
      | otherwise = case acc of
          []     -> go [[c]] cs
          (a:as) -> go ((c:a):as) cs
parseGistFile _ _ = Nothing

-- | Extract title from the first markdown heading
extractTitle :: T.Text -> T.Text
extractTitle body =
  case filter (T.isPrefixOf "# ") (T.lines body) of
    (h:_) -> T.strip (T.drop 2 h)
    []     -> "Untitled"

-- | GET a gist by ID
gistGet :: AppConfig -> IO (Either DiskWiseError Value)
gistGet config = do
  let gistId = configGistId config
  if T.null gistId
    then pure (Left (GitHubApiError "No gist ID configured"))
    else do
      manager <- newManager tlsManagerSettings
      let url = "https://api.github.com/gists/" <> T.unpack gistId
          token = configWikiToken config
          baseHeaders =
            [ ("User-Agent", "diskwise")
            , ("Accept", "application/vnd.github.v3+json")
            ]
          headers = if T.null token
            then baseHeaders
            else baseHeaders <> [(hAuthorization, "token " <> TE.encodeUtf8 token)]
      initialRequest <- parseRequest url
        `catch` (\(e :: SomeException) -> error $ "Bad URL: " <> url <> " " <> show e)
      let request = initialRequest { requestHeaders = headers }
      response <- httpLbs request manager
        `catch` (\(e :: SomeException) -> pure $ error $ "HTTP error: " <> show e)
      let status = statusCode (responseStatus response)
      if status >= 200 && status < 300
        then case eitherDecode (responseBody response) of
          Right val -> pure (Right val)
          Left err  -> pure (Left (ParseError (T.pack err)))
        else pure (Left (GitHubApiError ("GitHub API returned status " <> T.pack (show status))))

-- | PATCH a gist (for creating/updating files)
gistPatch :: AppConfig -> Value -> IO (Either DiskWiseError Value)
gistPatch config body = do
  let token = configWikiToken config
      gistId = configGistId config
  if T.null token
    then pure (Left (GitHubApiError "DISKWISE_WIKI_TOKEN not set. Set this environment variable to a classic GitHub PAT with gist access."))
    else if T.null gistId
    then pure (Left (GitHubApiError "No gist ID configured"))
    else do
      manager <- newManager tlsManagerSettings
      let url = "https://api.github.com/gists/" <> T.unpack gistId
      initialRequest <- parseRequest url
        `catch` (\(e :: SomeException) -> error $ "Bad URL: " <> url <> " " <> show e)
      let request = initialRequest
            { method = "PATCH"
            , requestHeaders =
                [ ("User-Agent", "diskwise")
                , ("Accept", "application/vnd.github.v3+json")
                , (hContentType, "application/json")
                , (hAuthorization, "token " <> TE.encodeUtf8 token)
                ]
            , requestBody = RequestBodyLBS (encode body)
            }
      response <- httpLbs request manager
        `catch` (\(_ :: SomeException) ->
          pure $ error "HTTP request failed")
      let status = statusCode (responseStatus response)
      if status >= 200 && status < 300
        then case eitherDecode (responseBody response) of
          Right val -> pure (Right val)
          Left _    -> pure (Right Null)
        else pure (Left (GitHubApiError
          ("GitHub API returned status " <> T.pack (show status))))
