{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)

import DiskWise.Types
import DiskWise.CLI

data Options = Options
  { optScanPaths  :: [FilePath]
  , optMinSizeMB  :: Integer
  , optModel      :: String
  , optApiKey     :: Maybe String
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
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

-- Hardcoded wiki config (user configures nothing)
wikiOwner :: T.Text
wikiOwner = "natanh"

wikiRepo :: T.Text
wikiRepo = "disk-wise-wiki"

wikiToken :: T.Text
wikiToken = ""  -- Set via DISKWISE_WIKI_TOKEN env var

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "AI-powered disk cleanup with shared wiki knowledge"
   <> header "diskwise - collaborative disk cleanup intelligence"
    )

  -- Get API key from flag or environment
  apiKeyMaybe <- case optApiKey opts of
    Just key -> pure (Just key)
    Nothing  -> lookupEnv "ANTHROPIC_API_KEY"
  let apiKey = maybe "" T.pack apiKeyMaybe

  -- Get wiki token from environment
  wikiTokenEnv <- lookupEnv "DISKWISE_WIKI_TOKEN"
  let token = maybe wikiToken T.pack wikiTokenEnv

  home <- maybe "$HOME" id <$> lookupEnv "HOME"
  let scanPaths = case optScanPaths opts of
        [] -> [home]
        ps -> ps

  let config = AppConfig
        { configApiKey     = apiKey
        , configWikiOwner  = wikiOwner
        , configWikiRepo   = wikiRepo
        , configWikiToken  = token
        , configScanPaths  = scanPaths
        , configMinSizeMB  = optMinSizeMB opts
        , configModel      = T.pack (optModel opts)
        }

  runApp config
