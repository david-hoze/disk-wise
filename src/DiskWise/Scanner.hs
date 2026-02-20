{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiskWise.Scanner
  ( scanSystem
  , runCleanupAction
  , validateAction
  , ValidationResult(..)
  , parseFindings
  , toMingwPath
  ) where

import Control.Exception (catch, SomeException)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesPathExist, findExecutable)
import System.Environment (lookupEnv)
import System.Process (readCreateProcessWithExitCode, proc)
import System.Exit (ExitCode(..))

import DiskWise.Types

-- | Convert a Windows path like "C:\Users\foo" to MINGW path "/c/Users/foo"
-- Also handles paths that are already MINGW-style (no-op).
toMingwPath :: FilePath -> FilePath
toMingwPath (drive:':':'\\':rest) =
  '/' : toLower drive : '/' : map bsToFs rest
  where bsToFs '\\' = '/'
        bsToFs c    = c
toMingwPath (drive:':':'/':rest) =
  '/' : toLower drive : '/' : rest
toMingwPath other = other

-- | Run a comprehensive system scan and return results as text.
-- Only gathers raw filesystem data — analysis is left to Claude + wiki.
scanSystem :: AppConfig -> IO T.Text
scanSystem config = do
  rawHome <- maybe "/tmp" id <$> lookupEnv "HOME"
  let home = toMingwPath rawHome
      paths = map toMingwPath (configScanPaths config)
  sections <- mapM gatherSection (scanSections home paths)
  pure $ T.unlines sections

data ScanSection = ScanSection
  { sectionName    :: T.Text
  , sectionCommand :: String
  }

-- | Generic scan sections — no tool-specific knowledge, just raw filesystem data.
scanSections :: FilePath -> [FilePath] -> [ScanSection]
scanSections _home paths =
  [ ScanSection "Disk Usage Overview" "df -h"
  , ScanSection "Largest directories" $
      "du -sh " <> unwords (concatMap (\p -> [p <> "/*/", p <> "/.*/"])  paths)
        <> " 2>/dev/null | sort -rh | head -50"
  , ScanSection "Large files (>100MB)" $
      "find " <> unwords paths
        <> " -maxdepth 5 -type f -size +100M 2>/dev/null | head -40 | xargs du -sh 2>/dev/null"
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
  (_, out, _) <- readCreateProcessWithExitCode (proc "sh" ["-c", cmd]) ""
    `catch` (\(_ :: SomeException) -> pure (ExitFailure 1, "(command failed)", ""))
  pure (T.pack out)

-- | Execute a cleanup action (with confirmation already obtained)
-- Uses sh -c so MINGW paths and redirections work correctly.
runCleanupAction :: CleanupAction -> IO (Either T.Text T.Text)
runCleanupAction action = do
  TIO.putStrLn $ "Running: " <> actionDescription action
  (exit, out, err) <- readCreateProcessWithExitCode
    (proc "sh" ["-c", T.unpack (actionCommand action)]) ""
  case exit of
    ExitSuccess   -> pure $ Right ("Done: " <> actionDescription action
                                   <> "\n" <> T.pack out)
    ExitFailure _ -> pure $ Left (T.pack err <> T.pack out)

-- | Result of validating a cleanup action before execution
data ValidationResult = ValidationResult
  { validationOk       :: Bool      -- ^ True if action looks safe to present
  , validationWarnings :: [T.Text]  -- ^ Warnings to show the user (empty = all clear)
  } deriving (Show, Eq)

-- | Validate a cleanup action before presenting it to the user.
-- Checks that referenced paths exist and command binaries are available.
validateAction :: CleanupAction -> IO ValidationResult
validateAction action = do
  let cmd = T.unpack (actionCommand action)
      ws = words cmd
      binary = case ws of
        (b:_) -> Just b
        []    -> Nothing
      -- Extract paths from the command (tokens starting with / or ~)
      paths = filter (\w -> take 1 w == "/" || take 1 w == "~") ws
  binaryWarns <- case binary of
    Nothing -> pure ["Empty command"]
    Just b  -> do
      found <- findExecutable b
      pure $ case found of
        Just _  -> []
        Nothing -> ["Binary not found: " <> T.pack b]
  pathWarns <- concat <$> mapM checkPath paths
  let allWarns = binaryWarns <> pathWarns
  pure ValidationResult
    { validationOk       = null allWarns
    , validationWarnings = allWarns
    }
  where
    checkPath p = do
      exists <- doesPathExist p
      pure $ if exists then [] else ["Path not found: " <> T.pack p]

-- | Best-effort parsing of scan output (du/find lines) into structured findings.
-- The @minBytes@ parameter filters out findings smaller than the threshold.
parseFindings :: Integer -> T.Text -> [Finding]
parseFindings minBytes scanOutput =
  filter (\f -> findingSize f >= minBytes) $
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
      | any (`T.isInfixOf` lp) ["/tmp", "/temp", "appdata/local/temp"] = "temp"
      | any (`T.isInfixOf` lp) ["dist-newstyle", ".stack-work", "node_modules", "target/"] = "build-artifact"
      | any (`T.isInfixOf` lp) ["docker"]                        = "docker"
      | any (`T.isInfixOf` lp) [".npm", "pip", "yarn", "cabal", ".stack", ".ghcup", "portablehaskell"] = "package-manager"
      | any (`T.isInfixOf` lp) ["appdata"]                       = "app-data"
      | otherwise = "other"
      where lp = T.toLower path
