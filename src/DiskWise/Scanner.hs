{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Scanner
  ( scanSystem
  , runCleanupAction
  , parseFindings
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess, readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

import DiskWise.Types

-- | Run a comprehensive system scan and return results as text
scanSystem :: AppConfig -> IO T.Text
scanSystem config = do
  sections <- mapM gatherSection (scanSections (configScanPaths config))
  pure $ T.unlines sections

data ScanSection = ScanSection
  { sectionName    :: T.Text
  , sectionCommand :: String
  }

scanSections :: [FilePath] -> [ScanSection]
scanSections paths =
  [ ScanSection "Disk Usage Overview" "df -h"
  , ScanSection "Largest directories" $
      "du -sh " <> unwords paths <> " 2>/dev/null | sort -rh | head -20"
  , ScanSection "Large files (>100MB)" $
      "find " <> unwords paths
        <> " -type f -size +100M -exec ls -lh {} \\; 2>/dev/null | head -30"
  , ScanSection "Old files (>90 days, >10MB)" $
      "find " <> unwords paths
        <> " -type f -mtime +90 -size +10M -exec ls -lh {} \\; 2>/dev/null | head -30"
  , ScanSection "Package manager caches"
      "du -sh ~/.npm ~/.cache/pip ~/.cache/yarn /var/cache/apt \
      \~/.cabal/store ~/.stack ~/.local/state/cabal 2>/dev/null"
  , ScanSection "Docker usage"
      "docker system df 2>/dev/null || echo 'Docker not found'"
  , ScanSection "Temp directories"
      "du -sh /tmp /var/tmp ~/.cache 2>/dev/null"
  , ScanSection "Log files"
      "find /var/log -type f -size +10M -exec ls -lh {} \\; 2>/dev/null | head -10"
  , ScanSection "Haskell-specific"
      "du -sh ~/.ghcup ~/.cabal ~/.stack dist-newstyle .stack-work 2>/dev/null"
  ]

gatherSection :: ScanSection -> IO T.Text
gatherSection section = do
  output <- runCommand (sectionCommand section)
  pure $ T.unlines
    [ "=== " <> sectionName section <> " ==="
    , output
    , ""
    ]

runCommand :: String -> IO T.Text
runCommand cmd = do
  result <- readProcess "sh" ["-c", cmd] ""
    `catch` (\(_ :: SomeException) -> pure "(command failed)")
  pure (T.pack result)

-- | Execute a cleanup action (with confirmation already obtained)
runCleanupAction :: CleanupAction -> IO (Either T.Text T.Text)
runCleanupAction action = do
  TIO.putStrLn $ "Running: " <> actionDescription action
  (exit, out, err) <- readCreateProcessWithExitCode
    (shell (T.unpack (actionCommand action))) ""
  case exit of
    ExitSuccess   -> pure $ Right ("Done: " <> actionDescription action
                                   <> "\n" <> T.pack out)
    ExitFailure _ -> pure $ Left (T.pack err <> T.pack out)

-- | Best-effort parsing of scan output (du/find lines) into structured findings
parseFindings :: T.Text -> [Finding]
parseFindings scanOutput =
  concatMap parseLine (T.lines scanOutput)
  where
    parseLine line
      | T.isPrefixOf "===" line = []  -- section header
      | T.null (T.strip line) = []    -- blank line
      | otherwise = case parseDuLine line of
          Just f  -> [f]
          Nothing -> case parseLsLine line of
            Just f  -> [f]
            Nothing -> []

    -- Parse "123M\t/some/path" or "1.2G\t/some/path" (du output)
    parseDuLine line =
      case T.breakOn "\t" (T.strip line) of
        (sizeStr, rest)
          | not (T.null rest) && not (T.null sizeStr) ->
              let path = T.strip (T.drop 1 rest)
                  bytes = parseSizeStr sizeStr
              in if bytes > 0 && not (T.null path)
                 then Just Finding
                   { findingPath     = T.unpack path
                   , findingSize     = bytes
                   , findingCategory = categorize path
                   , findingSummary  = sizeStr <> " at " <> path
                   }
                 else Nothing
          | otherwise -> Nothing

    -- Parse ls -lh output lines (e.g., "-rw-r--r-- 1 user group 1.2G Jan 15 file.iso")
    parseLsLine line =
      let ws = T.words (T.strip line)
      in if length ws >= 9 && T.isPrefixOf "-" (head ws)
         then let sizeStr = ws !! 4
                  path = T.unwords (drop 8 ws)
                  bytes = parseSizeStr sizeStr
              in if bytes > 0 && not (T.null path)
                 then Just Finding
                   { findingPath     = T.unpack path
                   , findingSize     = bytes
                   , findingCategory = categorize path
                   , findingSummary  = sizeStr <> " at " <> path
                   }
                 else Nothing
         else Nothing

    -- Parse human-readable size strings like "1.2G", "500M", "15K"
    parseSizeStr :: T.Text -> Integer
    parseSizeStr s =
      let (numPart, suffix) = T.span (\c -> c == '.' || (c >= '0' && c <= '9')) s
      in case reads (T.unpack numPart) :: [(Double, String)] of
           [(n, _)] -> case T.toUpper suffix of
             "K"  -> round (n * 1024)
             "M"  -> round (n * 1024 * 1024)
             "G"  -> round (n * 1024 * 1024 * 1024)
             "T"  -> round (n * 1024 * 1024 * 1024 * 1024)
             _    -> round n
           _ -> 0

    -- Categorize a path into a finding category
    categorize :: T.Text -> T.Text
    categorize path
      | any (`T.isInfixOf` lp) [".cache", "cache", "_cacache"]  = "cache"
      | any (`T.isInfixOf` lp) [".log", "/log/", "/logs/"]      = "log"
      | any (`T.isInfixOf` lp) ["/tmp", "/temp"]                 = "temp"
      | any (`T.isInfixOf` lp) ["dist-newstyle", ".stack-work", "node_modules", "target/"] = "build-artifact"
      | any (`T.isInfixOf` lp) ["docker"]                        = "docker"
      | any (`T.isInfixOf` lp) [".npm", "pip", "yarn", "cabal", ".stack", ".ghcup"] = "package-manager"
      | otherwise = "other"
      where lp = T.toLower path
