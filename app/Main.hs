{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO, try)
import Control.Monad (foldM, unless)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isLeft)
import Data.List (intercalate, isInfixOf, sort)
import Data.Text (pack, splitOn, unpack)
import Data.Version (showVersion)
import Exec (clrBlue, clrGreen, clrRed, clrYellow)
import Paths_chequera (version)
import QueryExtractor
import Steampipe
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
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
    Version -> putStrLn $ "chequera " ++ getVersion
    Test _ path -> do
      res <- runExceptT $ runSteampipeTests path
      case res of
        Left err -> do
          putStrLn $ prettyPrintError err
          exitWith $ ExitFailure 1
        Right NoErrors -> exitSuccess
        Right SomeErrors -> do
          exitWith $ ExitFailure 1

helpText :: String
helpText =
  "chequera "
    ++ getVersion
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

data TestsExitState = SomeErrors | NoErrors

runSteampipeTests :: FilePath -> ExceptT AppError IO TestsExitState
runSteampipeTests path = ExceptT $ try $ do
  putStrLn "Starting Steampipe service..."
  _ <- stopService
  _ <- startService
  putStrLn "Gathering the list of files..."
  files <- try $ findAllDocFiles Steampipe path
  case files of
    Left (err :: AppError) -> do
      putStrLn "Stopping Steampipe service..."
      _ <- stopService
      throwIO err
    Right (fs, excludes) -> do
      unless (null excludes) $ putStrLn $ clrYellow "Ignoring these files because of ignore flag/pattern: " ++ intercalate ", " excludes
      (eCount, tCount, eFiles) <-
        foldM
          ( \(errorCount :: Int, totalCount :: Int, errFiles :: [FilePath]) f -> do
              putStrLn $ "\n" ++ clrBlue "Testing: " ++ f
              res <- testFile Steampipe f
              case res of
                Left errs -> do
                  mapM_ putStrLn errs
                  pure (errorCount + length errs, totalCount + 1, f : errFiles)
                Right _ -> do
                  putStrLn $ clrGreen "All good!"
                  pure (errorCount, totalCount + 1, errFiles)
          )
          (0, 0, [])
          fs
      putStrLn "\n"
      putStrLn $ clrRed "Errors: " ++ show eCount
      putStrLn $ "Total files chequera'd: " ++ show tCount
      putStrLn "\n"
      unless (null eFiles) $ do
        putStrLn $ clrRed "These files have problematic queries: "
        mapM_ putStrLn eFiles
      putStrLn "Stopping Steampipe service..."
      _ <- stopService
      return $
        if null eFiles
          then NoErrors
          else SomeErrors

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

findAllDocFiles :: Pipeling -> FilePath -> IO ([FilePath], [FilePath])
findAllDocFiles pipeling dir = do
  ignoresString <- lookupEnv "CQ_IGNORE"
  let ignores = case ignoresString of
        Nothing -> []
        Just s -> map unpack $ splitOn (pack ",") $ pack s
  res <- readProcessWithExitCode "find" [dir, "-type", "f", "-name", if pipeling == Steampipe then "*.md" else "*.pp"] ""
  case res of
    (ExitSuccess, output, _) -> do
      let files = filter (not . patternInIgnoreList ignores) $ sort $ lines output
      let excludeFiles = filter (patternInIgnoreList ignores) $ sort $ lines output
      return (files, excludeFiles)
    (ExitFailure _, _, err) -> throwIO $ QueryExtractorError err
 where
  patternInIgnoreList :: [String] -> String -> Bool
  patternInIgnoreList ignorePatterns fileName = any (`isInfixOf` fileName) ignorePatterns

prettyPrintError :: AppError -> String
prettyPrintError err = case err of
  QueryExtractorError msg -> clrRed "Error when trying to extract queries: " ++ msg
  QueryExecError (QueryString q, e) -> clrRed "Error in SQL query: " ++ q ++ "Reason: " ++ e
  ExecError e -> clrRed "Error in shell command: " ++ e
  TimeoutError query -> clrRed "Timeout in query: " ++ query
  UnknownError e -> clrRed "Error:" ++ e

testFile :: Pipeling -> FilePath -> IO (Either [String] ())
testFile pipeling file = do
  res <- runQueriesFromFile pipeling file
  return $ case res of
    Left errs -> Left $ map prettyPrintError errs
    Right _ -> Right ()

getVersion :: String
getVersion = "v" ++ showVersion version