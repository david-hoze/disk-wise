{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Batch
  ( batchScan
  , batchAnalyze
  , batchCleanup
  , batchContribute
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (encode, eitherDecode, object, (.=), (.:), withObject, FromJSON(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

import System.IO (hPutStrLn, stderr)

import DiskWise.Types
import DiskWise.Wiki
import DiskWise.Claude
import DiskWise.Scanner

-- | Scan the system and output JSON findings to stdout
-- Output: { "scan_output": "...", "findings": [...] }
batchScan :: AppConfig -> IO ()
batchScan config = do
  hPutStrLn stderr "Scanning system..."
  scanOutput <- scanSystem config
  let findings = parseFindings scanOutput
  hPutStrLn stderr $ "Found " <> show (length findings) <> " items."
  BLC.putStrLn $ encode $ object
    [ "scan_output" .= scanOutput
    , "findings"    .= findings
    ]

-- | Take scan output (from stdin or file), fetch wiki, call Claude, output JSON advice
-- Input:  { "scan_output": "...", "findings": [...] }
-- Output: { "analysis": "...", "cleanup_actions": [...], "wiki_contributions": [...] }
batchAnalyze :: AppConfig -> FilePath -> IO ()
batchAnalyze config scanFile = do
  raw <- BL.readFile scanFile
  case eitherDecode raw of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse scan file: " <> err
      BLC.putStrLn $ encode $ object [ "error" .= T.pack err ]
    Right scanData -> do
      let scanOutput = sdScanOutput scanData
          findings   = sdFindings scanData

      -- Fetch wiki (graceful failure)
      hPutStrLn stderr "Fetching wiki..."
      wikiPages <- fetchWikiGracefully' config

      -- Match
      let matched = matchPages wikiPages findings
          matchedPaths = concatMap (map findingPath . snd) matched
          novel = filter (\f -> findingPath f `notElem` matchedPaths) findings

      hPutStrLn stderr $ "Wiki matched " <> show (length matched) <> " page(s)."
      hPutStrLn stderr $ show (length novel) <> " novel finding(s)."

      -- Call Claude
      hPutStrLn stderr "Calling Claude..."
      result <- investigate config scanOutput matched novel
      case result of
        Left err -> do
          hPutStrLn stderr $ "Claude error: " <> show err
          BLC.putStrLn $ encode $ object [ "error" .= T.pack (show err) ]
        Right advice ->
          BLC.putStrLn $ encode advice

-- | Execute a single cleanup action from JSON argument
-- Input:  JSON string of a CleanupAction
-- Output: { "success": true/false, "message": "..." }
batchCleanup :: String -> IO ()
batchCleanup actionJson = do
  case eitherDecode (BLC.pack actionJson) of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse action: " <> err
      BLC.putStrLn $ encode $ object
        [ "success" .= False, "message" .= T.pack err ]
    Right action -> do
      hPutStrLn stderr $ "Running: " <> T.unpack (actionDescription action)
      result <- runCleanupAction action
      case result of
        Right msg -> BLC.putStrLn $ encode $ object
          [ "success" .= True, "message" .= msg ]
        Left err -> BLC.putStrLn $ encode $ object
          [ "success" .= False, "message" .= err ]

-- | Push a single wiki contribution from JSON argument
-- Input:  JSON string of a WikiContribution
-- Output: { "success": true/false, "message": "..." }
batchContribute :: AppConfig -> String -> IO ()
batchContribute config contribJson = do
  case eitherDecode (BLC.pack contribJson) of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse contribution: " <> err
      BLC.putStrLn $ encode $ object
        [ "success" .= False, "message" .= T.pack err ]
    Right contrib -> do
      let prefixed = contrib { contribSummary = prefixCommitMsg (contribSummary contrib) }
      hPutStrLn stderr $ "Pushing: " <> T.unpack (contribSummary prefixed)

      -- Need wiki pages for SHA lookups
      wikiPages <- fetchWikiGracefully' config
      result <- pushContribution config wikiPages prefixed
      case result of
        Right () -> BLC.putStrLn $ encode $ object
          [ "success" .= True, "message" .= ("Pushed: " <> T.pack (contribPath prefixed)) ]
        Left err -> BLC.putStrLn $ encode $ object
          [ "success" .= False, "message" .= T.pack (show err) ]

-- | Internal: scan data for JSON deserialization
data ScanData = ScanData
  { sdScanOutput :: T.Text
  , sdFindings   :: [Finding]
  }

instance FromJSON ScanData where
  parseJSON = withObject "ScanData" $ \o ->
    ScanData <$> o .: "scan_output"
             <*> o .: "findings"

-- | Fetch wiki pages silently, returning empty on failure
fetchWikiGracefully' :: AppConfig -> IO [WikiPage]
fetchWikiGracefully' config = do
  result <- fetchTree config
    `catch` (\(_ :: SomeException) -> pure (Left WikiNotAvailable))
  case result of
    Right pages -> do
      hPutStrLn stderr $ "Loaded " <> show (length pages) <> " wiki page(s)."
      pure pages
    Left err -> do
      hPutStrLn stderr $ "Wiki unavailable: " <> show err
      pure []
