{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)

import DiskWise.Types
import DiskWise.CLI
import DiskWise.Batch

data Command
  = Interactive CommonOpts
  | BatchScan CommonOpts
  | BatchAnalyze CommonOpts FilePath
  | BatchCleanup String
  | BatchContribute CommonOpts String

data CommonOpts = CommonOpts
  { optScanPaths  :: [FilePath]
  , optMinSizeMB  :: Integer
  , optModel      :: String
  , optApiKey     :: Maybe String
  } deriving (Show)

commonOptsParser :: Parser CommonOpts
commonOptsParser = CommonOpts
  <$> many (strOption
        ( long "scan-path"
       <> short 'p'
       <> metavar "DIR"
       <> help "Directory to scan (can repeat; default: ~)"
        ))
  <*> option auto
        ( long "min-size"
       <> short 'm'
       <> metavar "MB"
       <> value 50
       <> help "Minimum file size in MB to report (default: 50)"
        )
  <*> strOption
        ( long "model"
       <> value "claude-sonnet-4-20250514"
       <> metavar "MODEL"
       <> help "Claude model to use (default: claude-sonnet-4-20250514)"
        )
  <*> optional (strOption
        ( long "api-key"
       <> metavar "KEY"
       <> help "Anthropic API key (overrides ANTHROPIC_API_KEY env var)"
        ))

commandParser :: Parser Command
commandParser = subparser
  ( command "interactive" (info (Interactive <$> commonOptsParser)
      (progDesc "Interactive mode (default)"))
  <> command "scan" (info (BatchScan <$> commonOptsParser)
      (progDesc "Scan system and output JSON findings to stdout"))
  <> command "analyze" (info (BatchAnalyze <$> commonOptsParser
      <*> strArgument (metavar "SCAN_FILE" <> help "Path to scan output JSON file"))
      (progDesc "Analyze scan output with wiki + Claude, output JSON advice"))
  <> command "cleanup" (info (BatchCleanup
      <$> strArgument (metavar "ACTION_JSON" <> help "CleanupAction as JSON string"))
      (progDesc "Execute a single cleanup action from JSON"))
  <> command "contribute" (info (BatchContribute <$> commonOptsParser
      <*> strArgument (metavar "CONTRIB_JSON" <> help "WikiContribution as JSON string"))
      (progDesc "Push a single wiki contribution from JSON"))
  )
  <|> (Interactive <$> commonOptsParser)  -- default to interactive

-- Hardcoded wiki config
wikiOwner :: T.Text
wikiOwner = "david-hoze"

wikiRepo :: T.Text
wikiRepo = "disk-wise-wiki"

wikiToken :: T.Text
wikiToken = ""

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "AI-powered disk cleanup with shared wiki knowledge"
   <> header "diskwise - collaborative disk cleanup intelligence"
    )

  case cmd of
    Interactive opts -> do
      config <- buildConfig opts
      runApp config

    BatchScan opts -> do
      config <- buildConfig opts
      batchScan config

    BatchAnalyze opts scanFile -> do
      config <- buildConfig opts
      batchAnalyze config scanFile

    BatchCleanup actionJson ->
      batchCleanup actionJson

    BatchContribute opts contribJson -> do
      config <- buildConfig opts
      batchContribute config contribJson

buildConfig :: CommonOpts -> IO AppConfig
buildConfig opts = do
  apiKeyMaybe <- case optApiKey opts of
    Just key -> pure (Just key)
    Nothing  -> lookupEnv "ANTHROPIC_API_KEY"
  let apiKey = maybe "" T.pack apiKeyMaybe

  wikiTokenEnv <- lookupEnv "DISKWISE_WIKI_TOKEN"
  let token = maybe wikiToken T.pack wikiTokenEnv

  home <- maybe "$HOME" id <$> lookupEnv "HOME"
  let scanPaths = case optScanPaths opts of
        [] -> [home]
        ps -> ps

  pure AppConfig
    { configApiKey     = apiKey
    , configWikiOwner  = wikiOwner
    , configWikiRepo   = wikiRepo
    , configWikiToken  = token
    , configScanPaths  = scanPaths
    , configMinSizeMB  = optMinSizeMB opts
    , configModel      = T.pack (optModel opts)
    }
