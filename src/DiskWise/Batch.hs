{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Batch
  ( batchScan
  , batchAnalyze
  , batchCleanup
  , batchContribute
  , batchGarden
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
  let minBytes = configMinSizeMB config * 1024 * 1024
      findings = parseFindings minBytes scanOutput
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
batchCleanup :: AppConfig -> String -> IO ()
batchCleanup config actionJson = do
  case eitherDecode (BLC.pack actionJson) of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse action: " <> err
      BLC.putStrLn $ encode $ object
        [ "success" .= False, "message" .= T.pack err ]
    Right action -> do
      hPutStrLn stderr $ "Running: " <> T.unpack (actionDescription action)
      result <- runCleanupAction action
      let success = case result of { Right _ -> True; Left _ -> False }
      -- Record outcome against wiki page metadata (best-effort)
      case actionWikiRef action of
        Just wref -> do
          wikiPages <- fetchWikiGracefully' config
          recordOutcome config wikiPages (T.unpack wref) success
        Nothing -> pure ()
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

-- | Run the gardener: improve wiki quality using Opus 4.6
batchGarden :: AppConfig -> IO ()
batchGarden config = do
  hPutStrLn stderr "Fetching full wiki tree (including _meta/)..."
  treeResult <- fetchFullTree config
  case treeResult of
    Left err -> do
      hPutStrLn stderr $ "Failed to fetch wiki: " <> show err
      pure ()
    Right allPages -> do
      hPutStrLn stderr $ "Loaded " <> show (length allPages) <> " page(s)."
      identity <- agentIdentity
      gardenLoop config allPages identity 0 []

-- | Gardening loop: improve wiki until convergence (max 5 passes)
gardenLoop :: AppConfig -> [WikiPage] -> T.Text -> Int -> [T.Text] -> IO ()
gardenLoop config allPages identity passNum passSummaries = do
  let maxPasses = 5 :: Int
  if passNum >= maxPasses
    then do
      hPutStrLn stderr "Max gardening passes reached."
      pushGardenSessionLog config allPages identity passSummaries
    else do
      let (contentPages, metaPages) = partitionPages allPages
      hPutStrLn stderr $ "Gardening pass " <> show (passNum + 1) <> "..."
      let userPrompt = buildGardenPrompt contentPages metaPages identity
          sysPrompt = buildGardenSystemPrompt
      result <- callClaudeGarden config sysPrompt userPrompt
      case result of
        Left err -> do
          hPutStrLn stderr $ "Gardening error: " <> show err
          pushGardenSessionLog config allPages identity passSummaries
        Right text -> case parseRefactorResult text of
          Left err -> do
            hPutStrLn stderr $ "Parse error: " <> show err
            pushGardenSessionLog config allPages identity passSummaries
          Right refResult -> do
            hPutStrLn stderr $ "  " <> T.unpack (refactorSummary refResult)
            if refactorDone refResult || null (refactorContributions refResult)
              then do
                hPutStrLn stderr "Wiki converged — no more improvements needed."
                pushGardenSessionLog config allPages identity
                  (passSummaries <> [refactorSummary refResult])
              else do
                -- Push each gardening contribution
                mapM_ (pushGardenContrib config allPages) (refactorContributions refResult)
                -- Re-fetch full tree for next pass (includes freshly written _meta/)
                freshResult <- fetchFullTree config
                case freshResult of
                  Left _ -> do
                    hPutStrLn stderr "Failed to re-fetch wiki after push."
                    pushGardenSessionLog config allPages identity
                      (passSummaries <> [refactorSummary refResult])
                  Right freshPages ->
                    gardenLoop config freshPages identity (passNum + 1)
                      (passSummaries <> [refactorSummary refResult])

-- | Partition pages into content pages and _meta/ pages
partitionPages :: [WikiPage] -> ([WikiPage], [WikiPage])
partitionPages pages =
  ( filter (not . isMeta) pages
  , filter isMeta pages
  )
  where
    isMeta page = T.isPrefixOf "_meta/" (T.pack (pageRelPath page))

-- | Push a gardening contribution (auto-approved)
pushGardenContrib :: AppConfig -> [WikiPage] -> WikiContribution -> IO ()
pushGardenContrib config pages contrib = do
  let prefixed = contrib { contribSummary = prefixGardenerMsg (contribSummary contrib) }
  result <- pushContribution config pages prefixed
  case result of
    Right () -> hPutStrLn stderr $ "  Pushed: " <> contribPath prefixed
    Left err -> hPutStrLn stderr $ "  Failed: " <> contribPath prefixed
                                <> " — " <> show err

-- | Push a final session log to _meta/gardening-log.md
pushGardenSessionLog :: AppConfig -> [WikiPage] -> T.Text -> [T.Text] -> IO ()
pushGardenSessionLog config pages identity summaries = do
  let logEntry = T.unlines $
        [ "- 2026-02-20: Gardening session (" <> identity <> ")" ] <>
        map (\s -> "  - " <> s) summaries
      existingMeta = filter (\p -> pageRelPath p == "_meta/gardening-log.md") pages
      (cType, content) = case existingMeta of
        (existing:_) -> (AmendPage, pageBody existing <> "\n" <> logEntry)
        []           -> (CreatePage, T.unlines
          [ "# Gardening Log"
          , ""
          , "Session history for the DiskWise wiki gardener."
          , ""
          , logEntry
          ])
      contrib = WikiContribution
        { contribType    = cType
        , contribPath    = "_meta/gardening-log.md"
        , contribContent = content
        , contribSummary = prefixGardenerMsg "update gardening session log"
        }
  result <- pushContribution config pages contrib
  case result of
    Right () -> hPutStrLn stderr "Gardening session log updated."
    Left err -> hPutStrLn stderr $ "Failed to update session log: " <> show err

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
