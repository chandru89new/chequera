{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try)
import Control.Monad (foldM, unless)
import Data.Either (isLeft)
import Data.List (isInfixOf, sort)
import Data.Text (pack, splitOn, unpack)
import Data.Version (showVersion)
import Exec (clrBlue, clrGreen, clrRed, clrYellow)
import Paths_chequera (version)
import QueryExtractor
import Steampipe
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Types

main :: IO ()
main = do
  args <- getArgs
  let command = getCommand args
  case command of
    Invalid -> do
      putStrLn "I didn't quite understand that. Try `chequera help`."
    Help -> do
      putStrLn helpText
    Version -> putStrLn $ "chequera v" ++ showVersion version
    Test pipeling path -> do
      _ <- startService
      files <- findAllDocFiles pipeling path
      (eCount, sCount, tCount, eFiles) <-
        foldM
          ( \(errorCount :: Int, successCount :: Int, totalCount :: Int, errFiles :: [FilePath]) f -> do
              putStrLn $ "\n" ++ clrBlue "Testing: " ++ f
              res <- testFile pipeling f
              case res of
                Left errs -> do
                  mapM_ putStrLn errs
                  pure (errorCount + length errs, successCount, totalCount + 1, f : errFiles)
                Right _ -> do
                  putStrLn $ clrGreen "All good!"
                  pure (errorCount, successCount + 1, totalCount + 1, errFiles)
          )
          (0, 0, 0, [])
          files
      putStrLn "\n"
      putStrLn $ clrRed "Errors: " ++ show eCount
      putStrLn $ clrGreen "Successes: " ++ show sCount
      putStrLn $ "Total: " ++ show tCount
      putStrLn "\n"
      unless (null eFiles) $ do
        putStrLn $ clrRed "These files have problematic queries: "
        mapM_ putStrLn eFiles
      stopService

helpText :: String
helpText =
  "chequera v"
    ++ showVersion version
    ++ "\n\n"
    ++ "Usage: chequera <command> [args]\n\
       \Commands:\n\
       \  help - Show this help.\n\
       \  version - Show version info.\n\
       \  test --path <p> - Test queries in markdown files in path <p>. Use absolute paths. eg. `chequera test --path /Users/james/steampipe-plugin-aws/docs\n"

getCommand :: [String] -> Command
getCommand [] = Help
getCommand ("help" : _) = Help
getCommand ("test" : "--path" : p : _) = Test Steampipe p
-- getCommand ("powerpipe" : "--path" : p : _) = Test Powerpipe p
getCommand ("version" : _) = Version
getCommand _ = Invalid

runQueriesFromFile :: Pipeling -> FilePath -> IO (Either [AppError] ())
runQueriesFromFile pipeling file = do
  queries <- try $ extractQueriesFromFile pipeling file
  case queries of
    Left (err :: AppError) -> return $ Left [err]
    Right qs -> do
      res <- concatErrors $ mapConcurrently runQ qs
      case res of
        Left err -> return $ Left err
        Right _ -> return $ Right ()
 where
  concatErrors :: IO [Either AppError ()] -> IO (Either [AppError] ())
  concatErrors ios = do
    res <- ios
    return $ case filter isLeft res of
      [] -> Right ()
      errs ->
        Left $
          foldl
            ( \b e ->
                case e of
                  Left err -> b ++ [err]
                  Right _ -> b
            )
            []
            errs

  runQ :: QueryString -> IO (Either AppError ())
  runQ (QueryString q) = runQuery q

findAllDocFiles :: Pipeling -> FilePath -> IO [FilePath]
findAllDocFiles pipeling dir = do
  ignoresString <- lookupEnv "CQ_IGNORE"
  let ignores = case ignoresString of
        Nothing -> []
        Just s -> map unpack $ splitOn (pack ",") $ pack s
  unless (null ignores) $ putStrLn $ clrYellow "Ignoring: " ++ show ignores
  res <- readProcessWithExitCode "find" [dir, "-type", "f", "-name", if pipeling == Steampipe then "*.md" else "*.pp"] ""
  case res of
    (ExitSuccess, output, _) -> do
      let files = filter (not . patternInIgnoreList ignores) $ sort $ lines output
      return files
    (ExitFailure _, _, err) -> error err
 where
  patternInIgnoreList :: [String] -> String -> Bool
  patternInIgnoreList ignorePatterns fileName = any (`isInfixOf` fileName) ignorePatterns

prettyPrintError :: AppError -> String
prettyPrintError err = case err of
  QueryExtractorError msg -> clrRed "Error when trying to extract queries: " ++ msg
  QueryExecError (QueryString q, e) -> clrRed "Error in SQL query: " ++ q ++ "Reason: " ++ e
  ExecError e -> clrRed "Error in shell command: " ++ e
  TimeoutError query -> clrRed "Timeout in query: " ++ query
  UnknownError -> clrRed "Some weird shit happened."

testFile :: Pipeling -> FilePath -> IO (Either [String] ())
testFile pipeling file = do
  res <- runQueriesFromFile pipeling file
  return $ case res of
    Left errs -> Left $ map prettyPrintError errs
    Right _ -> Right ()