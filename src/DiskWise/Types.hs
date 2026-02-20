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
  , SkipReason(..)
  , CleanupOutcome(..)
  , PlatformInfo(..)
  , ContribDecision(..)
  , SessionEvent(..)
  , SessionLog(..)
  , emptySessionLog
  , addEvent
  , RefactorResult(..)
  , SessionSummary(..)
  , CommandStats(..)
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
  { pageRelPath      :: FilePath      -- ^ Path relative to wiki root (e.g. "tools/npm.md")
  , pageTopic        :: Text          -- ^ Topic extracted from path/title
  , pageTitle        :: Text          -- ^ Title from first heading or filename
  , pageBody         :: Text          -- ^ Full markdown body
  , pageSha          :: Text          -- ^ Git SHA needed for updates
  , pageLastVerified :: Maybe UTCTime -- ^ When this page was last verified on a real system
  , pageVerifyCount  :: Int           -- ^ How many agents have verified this page's advice
  , pageFailCount    :: Int           -- ^ How many times advice from this page led to failures
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
  , configGistId     :: Text        -- ^ GitHub Gist ID for gist backend (empty = use repo)
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

-- | Reason the user skipped a cleanup action
data SkipReason
  = TooRisky
  | NotNow
  | AlreadyHandled
  | NotApplicable
  | SkipReasonOther Text
  deriving (Show, Eq, Generic)

instance ToJSON SkipReason
instance FromJSON SkipReason

-- | Result of executing a cleanup action, with measured space data
data CleanupOutcome = CleanupOutcome
  { outcomeAction      :: CleanupAction
  , outcomeSuccess     :: Bool
  , outcomeMessage     :: Text
  , outcomeBytesFreed  :: Maybe Integer  -- ^ Measured difference in free space
  , outcomeExpected    :: Maybe Text     -- ^ What the wiki estimated (from actionSizeEstimate)
  , outcomePosition    :: Int            -- ^ Position in the list of proposed actions (0-indexed)
  , outcomeOrder       :: Int            -- ^ Actual execution order (0-indexed, among accepted)
  } deriving (Show, Eq, Generic)

instance ToJSON CleanupOutcome
instance FromJSON CleanupOutcome

-- | Platform information for correlating failures with environment
data PlatformInfo = PlatformInfo
  { platformOS     :: Text   -- ^ "linux", "darwin", "mingw64", etc.
  , platformArch   :: Text   -- ^ "x86_64", "aarch64", etc.
  , platformShell  :: Text   -- ^ "bash", "zsh", "sh", etc.
  } deriving (Show, Eq, Generic)

instance ToJSON PlatformInfo
instance FromJSON PlatformInfo

-- | How the user decided on a wiki contribution
data ContribDecision
  = ContribApproved
  | ContribSkipped
  | ContribEdited Text  -- ^ The user's modification note
  deriving (Show, Eq, Generic)

instance ToJSON ContribDecision
instance FromJSON ContribDecision

-- | An event that occurred during the session, for Claude to learn from
data SessionEvent
  = ActionExecuted CleanupOutcome       -- ^ action executed with outcome
  | ActionFailed CleanupOutcome         -- ^ action failed with outcome
  | ActionSkipped CleanupAction SkipReason -- ^ user declined + reason
  | ContribPushed WikiContribution ContribDecision -- ^ pushed + how user decided
  | ContribFailed WikiContribution Text -- ^ failed to push + error
  | UserFeedback Text                  -- ^ freeform feedback at end of session
  deriving (Show, Eq)

-- | Log of everything that happened in a session
data SessionLog = SessionLog
  { logScanOutput   :: Text
  , logFindings     :: [Finding]
  , logAdvice       :: Maybe ClaudeAdvice
  , logEvents       :: [SessionEvent]
  , logPlatform     :: PlatformInfo
  } deriving (Show, Eq)

-- | Create an empty session log
emptySessionLog :: SessionLog
emptySessionLog = SessionLog
  { logScanOutput = ""
  , logFindings   = []
  , logAdvice     = Nothing
  , logEvents     = []
  , logPlatform   = PlatformInfo "unknown" "unknown" "unknown"
  }

-- | Append an event to the session log
addEvent :: SessionLog -> SessionEvent -> SessionLog
addEvent sl ev = sl { logEvents = logEvents sl ++ [ev] }

-- | What Claude returns from a refactoring pass
data RefactorResult = RefactorResult
  { refactorContributions :: [WikiContribution]
  , refactorDone          :: Bool  -- ^ True if no meaningful improvements found
  , refactorSummary       :: Text  -- ^ What was improved this pass
  } deriving (Show, Eq, Generic)

instance ToJSON RefactorResult
instance FromJSON RefactorResult

-- | Summary of a session, for cross-session learning
data SessionSummary = SessionSummary
  { summaryTimestamp     :: UTCTime
  , summaryPlatform      :: PlatformInfo
  , summaryFindingCount  :: Int
  , summaryActionsRun    :: Int
  , summaryActionsFailed :: Int
  , summarySkipReasons   :: [(Text, SkipReason)]  -- ^ (action description, reason)
  , summaryBytesFreed    :: Maybe Integer
  , summaryUserFeedback  :: Maybe Text
  , summaryFailedCmds    :: [(Text, Text)]  -- ^ (command, error)
  , summaryCleanedPaths  :: [(FilePath, Integer)]  -- ^ (path, bytes freed) for regrowth tracking
  , summarySucceededCmds :: [Text]  -- ^ Commands that succeeded (for reliability tracking)
  } deriving (Show, Eq, Generic)

instance ToJSON SessionSummary
instance FromJSON SessionSummary

-- | Aggregated reliability statistics for a cleanup command
data CommandStats = CommandStats
  { cmdStatsCommand  :: Text   -- ^ The command string
  , cmdStatsSuccesses :: Int   -- ^ Times it succeeded
  , cmdStatsFailures  :: Int   -- ^ Times it failed
  , cmdStatsLastError :: Maybe Text  -- ^ Most recent error message
  } deriving (Show, Eq, Generic)

instance ToJSON CommandStats
instance FromJSON CommandStats
