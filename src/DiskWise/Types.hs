{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DiskWise.Types
  ( DiskEntry(..)
  , WikiPage(..)
  , WikiContribution(..)
  , ContribType(..)
  , CleanupAction(..)
  , ClaudeAdvice(..)
  , Finding(..)
  , AppConfig(..)
  , DiskWiseError(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | A disk entry found during scanning (unchanged from v1)
data DiskEntry = DiskEntry
  { entryPath    :: FilePath
  , entrySize    :: Integer       -- ^ Size in bytes
  , entryIsDir   :: Bool
  , entryModTime :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON DiskEntry
instance FromJSON DiskEntry

-- | A page fetched from the GitHub wiki
data WikiPage = WikiPage
  { pageRelPath :: FilePath      -- ^ Path relative to wiki root (e.g. "tools/npm.md")
  , pageTopic   :: Text          -- ^ Topic extracted from path/title
  , pageTitle   :: Text          -- ^ Title from first heading or filename
  , pageBody    :: Text          -- ^ Full markdown body
  , pageSha     :: Text          -- ^ Git SHA needed for updates
  } deriving (Show, Eq, Generic)

instance ToJSON WikiPage
instance FromJSON WikiPage

-- | Type of contribution to the wiki
data ContribType
  = CreatePage
  | AmendPage
  deriving (Show, Eq, Generic)

instance ToJSON ContribType
instance FromJSON ContribType

-- | A page to create or amend on the wiki
data WikiContribution = WikiContribution
  { contribType    :: ContribType
  , contribPath    :: FilePath    -- ^ Path in wiki repo
  , contribContent :: Text        -- ^ Markdown content
  , contribSummary :: Text        -- ^ Commit message
  } deriving (Show, Eq, Generic)

instance ToJSON WikiContribution
instance FromJSON WikiContribution

-- | A proposed cleanup action
data CleanupAction = CleanupAction
  { actionDescription  :: Text
  , actionCommand      :: Text
  , actionRiskLevel    :: Text        -- ^ e.g. "low", "medium", "high"
  , actionSizeEstimate :: Maybe Text  -- ^ e.g. "~2.3 GB"
  , actionWikiRef      :: Maybe Text  -- ^ Reference to wiki page if applicable
  } deriving (Show, Eq, Generic)

instance ToJSON CleanupAction
instance FromJSON CleanupAction

-- | What Claude returns after investigating
data ClaudeAdvice = ClaudeAdvice
  { adviceAnalysis       :: Text
  , adviceCleanupActions :: [CleanupAction]
  , adviceContributions  :: [WikiContribution]
  } deriving (Show, Eq, Generic)

instance ToJSON ClaudeAdvice
instance FromJSON ClaudeAdvice

-- | A structured finding from scanning
data Finding = Finding
  { findingPath     :: FilePath
  , findingSize     :: Integer      -- ^ Size in bytes
  , findingCategory :: Text         -- ^ e.g. "cache", "log", "build-artifact"
  , findingSummary  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Finding
instance FromJSON Finding

-- | Application configuration
data AppConfig = AppConfig
  { configApiKey     :: Text
  , configWikiOwner  :: Text        -- ^ GitHub owner of wiki repo
  , configWikiRepo   :: Text        -- ^ GitHub repo name for wiki
  , configWikiToken  :: Text        -- ^ GitHub token for wiki writes
  , configScanPaths  :: [FilePath]
  , configMinSizeMB  :: Integer
  , configModel      :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AppConfig
instance FromJSON AppConfig

-- | Errors that can occur in DiskWise
data DiskWiseError
  = GitHubApiError Text
  | ClaudeError Text
  | ParseError Text
  | WikiNotAvailable
  deriving (Show, Eq)
