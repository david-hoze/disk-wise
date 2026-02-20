{-# LANGUAGE OverloadedStrings #-}

module DiskWise.WikiRouter
  ( fetchTree
  , fetchFullTree
  , fetchPage
  , matchPages
  , matchPagesHeuristic
  , matchPagesWithClaude
  , createPage
  , updatePage
  , pushContribution
  , parsePagePatterns
  , parsePageToolNames
  , sanitizeContent
  , updatePageMeta
  , parseMetaComment
  , renderMetaComment
  , recordOutcome
  , PageMeta(..)
  , defaultPageMeta
  , deduplicateContribs
  ) where

import qualified Data.Text as T

import DiskWise.Types
import qualified DiskWise.Wiki as Wiki
import DiskWise.Wiki (PageMeta(..))
import qualified DiskWise.WikiGist as Gist

-- | Check whether the gist backend should be used.
useGist :: AppConfig -> Bool
useGist config = not (T.null (configGistId config))

-- Re-export pure functions directly from Wiki (backend-independent)
matchPages :: [WikiPage] -> [Finding] -> [(WikiPage, [Finding])]
matchPages = Wiki.matchPages

matchPagesHeuristic :: [WikiPage] -> [Finding] -> [(WikiPage, [Finding])]
matchPagesHeuristic = Wiki.matchPagesHeuristic

matchPagesWithClaude :: (T.Text -> T.Text -> IO (Either DiskWiseError T.Text))
                     -> [WikiPage] -> [Finding]
                     -> IO [(WikiPage, [Finding])]
matchPagesWithClaude = Wiki.matchPagesWithClaude

parsePagePatterns :: T.Text -> [T.Text]
parsePagePatterns = Wiki.parsePagePatterns

parsePageToolNames :: WikiPage -> [T.Text]
parsePageToolNames = Wiki.parsePageToolNames

sanitizeContent :: T.Text -> T.Text
sanitizeContent = Wiki.sanitizeContent

updatePageMeta :: Bool -> IO (WikiPage -> WikiPage)
updatePageMeta = Wiki.updatePageMeta

parseMetaComment :: T.Text -> (PageMeta, T.Text)
parseMetaComment = Wiki.parseMetaComment

renderMetaComment :: PageMeta -> T.Text
renderMetaComment = Wiki.renderMetaComment

defaultPageMeta :: PageMeta
defaultPageMeta = Wiki.defaultPageMeta

deduplicateContribs :: [WikiPage] -> [WikiContribution] -> [WikiContribution]
deduplicateContribs = Wiki.deduplicateContribs

-- Routed functions: delegate to Wiki or WikiGist based on config

fetchTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchTree config
  | useGist config = Gist.fetchTree config
  | otherwise      = Wiki.fetchTree config

fetchFullTree :: AppConfig -> IO (Either DiskWiseError [WikiPage])
fetchFullTree config
  | useGist config = Gist.fetchFullTree config
  | otherwise      = Wiki.fetchFullTree config

fetchPage :: AppConfig -> FilePath -> IO (Either DiskWiseError WikiPage)
fetchPage config path
  | useGist config = Gist.fetchPage config path
  | otherwise      = Wiki.fetchPage config path

createPage :: AppConfig -> WikiContribution -> IO (Either DiskWiseError ())
createPage config contrib
  | useGist config = Gist.createPage config contrib
  | otherwise      = Wiki.createPage config contrib

updatePage :: AppConfig -> WikiPage -> WikiContribution -> IO (Either DiskWiseError ())
updatePage config page contrib
  | useGist config = Gist.updatePage config page contrib
  | otherwise      = Wiki.updatePage config page contrib

pushContribution :: AppConfig -> [WikiPage] -> WikiContribution -> IO (Either DiskWiseError ())
pushContribution config pages contrib
  | useGist config = Gist.pushContribution config pages contrib
  | otherwise      = Wiki.pushContribution config pages contrib

recordOutcome :: AppConfig -> [WikiPage] -> FilePath -> Bool -> IO ()
recordOutcome config pages wikiRef success
  | useGist config = Gist.recordOutcome config pages wikiRef success
  | otherwise      = Wiki.recordOutcome config pages wikiRef success
