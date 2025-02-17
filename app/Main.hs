{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.List (intercalate, isInfixOf, sort)
import Data.Text (pack, splitOn, unpack)
import Data.Version (showVersion)
import Exec (logError, logInfo, logSuccess, logWarning)
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
      res <- runAppM $ runSteampipeTests path
      case res of
        Left err -> do
          _ <- putStrLn $ showAppError err
          exitWith $ ExitFailure 1
        Right TestExitFailure -> do
          exitWith $ ExitFailure 1
        Right TestExitSuccess -> do
          exitSuccess

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

findAllDocFiles :: Pipeling -> FilePath -> AppM ([FilePath], [FilePath])
findAllDocFiles pipeling dir = ExceptT $ do
  ignoresString <- lookupEnv "CQ_IGNORE"
  let ignores = case ignoresString of
        Nothing -> []
        Just s -> map unpack $ splitOn (pack ",") $ pack s
  res <- readProcessWithExitCode "find" [dir, "-type", "f", "-name", if pipeling == Steampipe then "*.md" else "*.pp"] ""
  return $ case res of
    (ExitSuccess, output, _) ->
      let files = filter (not . patternInIgnoreList ignores) $ sort $ lines output
          excludeFiles = filter (patternInIgnoreList ignores) $ sort $ lines output
       in Right (files, excludeFiles)
    (ExitFailure _, _, err) -> Left (InvalidPath err)
 where
  patternInIgnoreList :: [String] -> String -> Bool
  patternInIgnoreList ignorePatterns fileName = any (`isInfixOf` fileName) ignorePatterns

showAppError :: AppError -> String
showAppError err = case err of
  QueryExtractionError msg -> logError msg
  ExecError e -> logError e
  TimeoutError cmd -> logError ("Timed out: " ++ (unwords . words) cmd)
  InvalidPath e -> logError ("Invalid path: " ++ e)
  UnknownError e -> logError e

getVersion :: String
getVersion = "v" ++ showVersion version

testFile :: Pipeling -> FilePath -> FileTestM ()
testFile pipeling file = do
  liftIO $ putStrLn $ "\n" ++ logInfo ("Checking: " ++ file)
  queries <- extractQueriesFromFile pipeling file
  res <- liftIO $ mapConcurrently testQuery queries
  let errs = concatErrors res
  ExceptT $
    pure $
      if null errs
        then Right ()
        else Left (QueryExecError errs)
 where
  testQuery :: QueryString -> IO (QueryString, Maybe QueryExecError')
  testQuery qs = do
    r <- runQuery qs
    pure $ case r of
      Left err -> (qs, Just err)
      Right _ -> (qs, Nothing)

  concatErrors :: [(QueryString, Maybe QueryExecError')] -> [(QueryString, QueryExecError')]
  concatErrors =
    foldl
      ( \acc (qs, merr) -> case merr of
          Just err -> (qs, err) : acc
          Nothing -> acc
      )
      []

showFileTestError :: FileTestError -> String
showFileTestError err = case err of
  ParseError' e -> logError e
  QueryExecError errs ->
    intercalate "\n" $
      map
        ( \(QueryString qs, e') ->
            let qstr = (unwords . words) qs
             in case e' of
                  QueryTimeout -> logError qstr ++ "\n" ++ logError "Query timed out.\n"
                  InvalidQuery e -> logError qstr ++ "\n" ++ logError e
                  UnknownQueryError e -> logError qstr ++ "\n" ++ logError e ++ "\n"
        )
        errs

runSteampipeTests :: FilePath -> AppM TestExitState
runSteampipeTests path = do
  liftIO $ putStrLn "Gathering the list of files..."
  (fs, excludes) <- findAllDocFiles Steampipe path
  unless (null excludes) $ liftIO $ putStrLn $ logWarning "Ignoring these files because of ignore flag/pattern: " ++ intercalate ", " excludes
  case fs of
    [] -> do
      liftIO $ putStrLn $ logInfo "No files to check."
      return TestExitSuccess
    _ -> do
      liftIO $ putStrLn $ logInfo "Starting Steampipe service..."
      stopService -- we stop any existing service. otherwise `startService` throws an error.
      startService
      errFiles <-
        foldM
          ( \acc f -> do
              res <- liftIO $ runFileTestM $ testFile Steampipe f
              liftIO $ case res of
                Left err -> do
                  putStrLn $ showFileTestError err
                  pure (f : acc)
                Right () -> do
                  putStrLn $ logSuccess "All good!"
                  pure acc
          )
          []
          fs
      liftIO $ do
        putStrLn "------------"
        putStrLn $ logInfo "Total files checked: " ++ show (length fs)
        if not . null $ errFiles
          then do
            putStrLn $ logWarning "There are errors in these files:"
            mapM_ putStrLn errFiles
          else
            putStrLn $ logSuccess "No errors!"
      stopService
      if null errFiles
        then return TestExitSuccess
        else return TestExitFailure