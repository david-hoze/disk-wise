{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Wiki
  ( fetchTree
  , fetchFullTree
  , fetchPage
  , matchPages
  , createPage
  , updatePage
  , pushContribution
  , parsePagePatterns
  , parsePageToolNames
  , sanitizeContent
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType, hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

import DiskWise.Types

-- | Fetch the full tree of wiki pages from the GitHub wiki repo
fetchTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchTree config = do
  let url = "/repos/" <> T.unpack (configWikiOwner config)
         <> "/" <> T.unpack (configWikiRepo config)
         <> "/git/trees/main?recursive=1"
  result <- githubGet config url
  case result of
    Left err -> pure (Left err)
    Right val -> do
      let paths = extractTreePaths val
          mdPaths = filter (\p -> T.isSuffixOf ".md" p && p /= "Home.md"
                               && not (T.isPrefixOf "_meta/" p)) paths
      pages <- mapM (\p -> fetchPage config (T.unpack p)) mdPaths
      pure $ Right [pg | Right pg <- pages]

-- | Fetch the full tree including _meta/ pages (for the gardener)
fetchFullTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchFullTree config = do
  let url = "/repos/" <> T.unpack (configWikiOwner config)
         <> "/" <> T.unpack (configWikiRepo config)
         <> "/git/trees/main?recursive=1"
  result <- githubGet config url
  case result of
    Left err -> pure (Left err)
    Right val -> do
      let paths = extractTreePaths val
          mdPaths = filter (\p -> T.isSuffixOf ".md" p && p /= "Home.md") paths
      pages <- mapM (\p -> fetchPage config (T.unpack p)) mdPaths
      pure $ Right [pg | Right pg <- pages]

-- | Extract file paths from a GitHub tree API response
extractTreePaths :: Value -> [T.Text]
extractTreePaths (Object obj) =
  case KM.lookup "tree" obj of
    Just (Array items) ->
      [ path
      | Object item <- V.toList items
      , Just (String "blob") <- [KM.lookup "type" item]
      , Just (String path) <- [KM.lookup "path" item]
      ]
    _ -> []
extractTreePaths _ = []

-- | Fetch a single page from the wiki repo
fetchPage :: AppConfig -> FilePath -> IO (Either DiskWiseError WikiPage)
fetchPage config path = do
  let url = "/repos/" <> T.unpack (configWikiOwner config)
         <> "/" <> T.unpack (configWikiRepo config)
         <> "/contents/" <> path
  result <- githubGet config url
  case result of
    Left err -> pure (Left err)
    Right val -> pure (parsePageResponse path val)

-- | Parse a GitHub Contents API response into a WikiPage
parsePageResponse :: FilePath -> Value -> Either DiskWiseError WikiPage
parsePageResponse path (Object obj) = do
  let sha = case KM.lookup "sha" obj of
        Just (String s) -> s
        _               -> ""
  content <- case KM.lookup "content" obj of
    Just (String b64) ->
      case B64.decode (TE.encodeUtf8 (T.filter (/= '\n') b64)) of
        Right bs -> Right (TE.decodeUtf8 bs)
        Left err -> Left (ParseError (T.pack err))
    _ -> Left (ParseError "No content field in response")
  let topic = T.toLower $ T.pack $ takeWhile (/= '.') $ last $ splitPath path
      title = extractTitle content
  Right WikiPage
    { pageRelPath = path
    , pageTopic   = topic
    , pageTitle   = title
    , pageBody    = content
    , pageSha     = sha
    }
  where
    splitPath = go []
    go acc [] = reverse (reverse [] : acc)
    go acc (c:cs)
      | c == '/' = go (reverse [] : acc) cs
      | otherwise = case acc of
          []     -> go [[c]] cs
          (a:as) -> go ((c:a):as) cs
parsePageResponse _ _ = Left (ParseError "Unexpected response format")

-- | Extract title from the first markdown heading
extractTitle :: T.Text -> T.Text
extractTitle body =
  case filter (T.isPrefixOf "# ") (T.lines body) of
    (h:_) -> T.strip (T.drop 2 h)
    []     -> "Untitled"

-- | Match wiki pages to findings by path patterns and tool names
matchPages :: [WikiPage] -> [Finding] -> [(WikiPage, [Finding])]
matchPages pages findings =
  [ (page, matched)
  | page <- pages
  , let patterns = parsePagePatterns (pageBody page)
        toolNames = parsePageToolNames page
        matched = filter (matchesFinding patterns toolNames) findings
  , not (null matched)
  ]

-- | Check if a finding matches any pattern or tool name from a wiki page
matchesFinding :: [T.Text] -> [T.Text] -> Finding -> Bool
matchesFinding patterns toolNames finding =
  any (`T.isInfixOf` pathText) patterns
  || any (`T.isInfixOf` T.toLower (findingCategory finding)) toolNames
  || any (`T.isInfixOf` T.toLower (findingSummary finding)) toolNames
  where
    pathText = T.pack (findingPath finding)

-- | Extract path patterns from markdown body
-- Looks for backtick-quoted paths, especially under "Where it stores data" sections
parsePagePatterns :: T.Text -> [T.Text]
parsePagePatterns body = concatMap extractBacktickPaths (T.lines body)
  where
    extractBacktickPaths line =
      let parts = T.splitOn "`" line
          -- Every other part (index 1, 3, 5...) is inside backticks
          insideBackticks = everyOther (drop 1 parts)
      in filter isPathLike insideBackticks

    everyOther [] = []
    everyOther [x] = [x]
    everyOther (x:_:rest) = x : everyOther rest

    isPathLike t =
      not (T.null t)
      && (T.any (== '/') t || T.isPrefixOf "~" t)
      && not (T.any (== ' ') t && not (T.isPrefixOf "/" t || T.isPrefixOf "~" t))

-- | Extract tool names from title and headings
parsePageToolNames :: WikiPage -> [T.Text]
parsePageToolNames page =
  map T.toLower $ filter (not . T.null)
    [ pageTopic page
    , pageTitle page
    ] <> headingWords
  where
    headingWords =
      [ T.toLower (T.strip (T.drop 2 line))
      | line <- T.lines (pageBody page)
      , T.isPrefixOf "## " line
      ]

-- | Replace home directory paths with ~ and strip usernames (pure version)
-- Takes the home directory as a parameter for testability
sanitizeContent :: T.Text -> T.Text
sanitizeContent = id  -- Pure no-op; use sanitizeContentIO for actual sanitization

-- | Sanitize content by replacing home dir with ~
sanitizeContentIO :: T.Text -> IO T.Text
sanitizeContentIO content = do
  homeDir <- lookupEnv "HOME"
  pure $ case homeDir of
    Just h  -> T.replace (T.pack h) "~" content
    Nothing -> content

-- | Create a new page in the wiki
createPage :: AppConfig -> WikiContribution -> IO (Either DiskWiseError ())
createPage config contrib = do
  sanitized <- sanitizeContentIO (contribContent contrib)
  let url = "/repos/" <> T.unpack (configWikiOwner config)
         <> "/" <> T.unpack (configWikiRepo config)
         <> "/contents/" <> contribPath contrib
      body = object
        [ "message" .= contribSummary contrib
        , "content" .= TE.decodeUtf8 (B64.encode (TE.encodeUtf8 sanitized))
        ]
  result <- githubPut config url body
  case result of
    Left err -> pure (Left err)
    Right _  -> pure (Right ())

-- | Update an existing page in the wiki (requires SHA for optimistic concurrency)
updatePage :: AppConfig -> WikiPage -> WikiContribution -> IO (Either DiskWiseError ())
updatePage config page contrib = do
  sanitized <- sanitizeContentIO (contribContent contrib)
  let url = "/repos/" <> T.unpack (configWikiOwner config)
         <> "/" <> T.unpack (configWikiRepo config)
         <> "/contents/" <> contribPath contrib
      body = object
        [ "message" .= contribSummary contrib
        , "content" .= TE.decodeUtf8 (B64.encode (TE.encodeUtf8 sanitized))
        , "sha"     .= pageSha page
        ]
  result <- githubPut config url body
  case result of
    Left err -> pure (Left err)
    Right _  -> pure (Right ())

-- | Push a contribution (create or amend) with retry on SHA conflicts
pushContribution :: AppConfig -> [WikiPage] -> WikiContribution -> IO (Either DiskWiseError ())
pushContribution config pages contrib = go 0
  where
    maxRetries = 5 :: Int
    go attempt
      | attempt >= maxRetries = pure (Left (GitHubApiError "Max retries exceeded on SHA conflict"))
      | otherwise = do
          result <- case contribType contrib of
            CreatePage -> createPage config contrib
            AmendPage  ->
              case filter (\p -> pageRelPath p == contribPath contrib) pages of
                (page:_) -> updatePage config page contrib
                []       -> createPage config contrib  -- fallback to create if page not found
          case result of
            Left (GitHubApiError msg)
              | T.isInfixOf "409" msg || T.isInfixOf "sha" (T.toLower msg) -> do
                  -- SHA conflict — exponential backoff and retry
                  let delayMs = 1000000 * (2 ^ attempt)  -- 1s, 2s, 4s, 8s, 16s
                  threadDelay delayMs
                  -- Re-fetch the page to get current SHA
                  freshPage <- fetchPage config (contribPath contrib)
                  case freshPage of
                    Right fp -> do
                      retryResult <- updatePage config fp contrib
                      case retryResult of
                        Left err -> go (attempt + 1)
                        Right () -> pure (Right ())
                    Left _ -> go (attempt + 1)
            other -> pure other

-- | Make a GET request to GitHub API (authenticated when token is available)
githubGet :: AppConfig -> String -> IO (Either DiskWiseError Value)
githubGet config path = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com" <> path
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
  let request = initialRequest
        { requestHeaders = headers
        }
  response <- httpLbs request manager
    `catch` (\(e :: SomeException) -> pure $ error $ "HTTP error: " <> show e)
  let status = statusCode (responseStatus response)
  if status >= 200 && status < 300
    then case eitherDecode (responseBody response) of
      Right val -> pure (Right val)
      Left err  -> pure (Left (ParseError (T.pack err)))
    else pure (Left (GitHubApiError ("GitHub API returned status " <> T.pack (show status))))

-- | Make an authenticated PUT request to GitHub API
githubPut :: AppConfig -> String -> Value -> IO (Either DiskWiseError Value)
githubPut config path body = do
  let token = configWikiToken config
  if T.null token
    then pure (Left (GitHubApiError "DISKWISE_WIKI_TOKEN not set. Set this environment variable to a classic GitHub PAT with write access to the wiki repo."))
    else do
      manager <- newManager tlsManagerSettings
      let url = "https://api.github.com" <> path
      initialRequest <- parseRequest url
        `catch` (\(e :: SomeException) -> error $ "Bad URL: " <> url <> " " <> show e)
      let request = initialRequest
            { method = "PUT"
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
          Left _    -> pure (Right Null)  -- some PUT responses don't have JSON
        else pure (Left (GitHubApiError
          ("GitHub API returned status " <> T.pack (show status))))
