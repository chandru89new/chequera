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
import Data.Either (isRight)
import Data.List (foldl', intercalate, isInfixOf, sort)
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

runSteampipeTests :: FilePath -> AppM TestExitState
runSteampipeTests path = do
  liftIO $ putStrLn "Gathering the list of files..."
  (fs, excludes) <- findAllDocFiles Steampipe path
  unless (null excludes) $ liftIO $ putStrLn $ clrYellow "Ignoring these files because of ignore flag/pattern: " ++ intercalate ", " excludes
  case fs of
    [] -> do
      liftIO $ putStrLn $ clrYellow "No files to check."
      return TestExitSuccess
    _ -> do
      liftIO $ putStrLn "Starting Steampipe service..."
      stopService -- we stop any existing service. otherwise `startService` throws an error.
      startService
      errFiles <- foldM testAndCollect [] fs
      liftIO $ putStrLn $ "Total files checked: " ++ show (length fs)
      liftIO $ unless (null errFiles) $ do
        putStrLn $ clrRed "There are errors in these files:"
        mapM_ putStrLn errFiles
      stopService
      if null errFiles
        then return TestExitSuccess
        else return TestExitFailure
 where
  testAndCollect :: [FilePath] -> FilePath -> AppM [FilePath]
  testAndCollect errFs f = do
    liftIO $ putStrLn $ "\n" ++ clrBlue "Testing: " ++ f
    ftr <- runQueriesFromFile Steampipe f
    liftIO $ case ftr of
      NoErrors -> do
        putStrLn $ clrGreen "All good."
        pure errFs
      QueryExecutionErrors errs -> do
        mapM_
          ( \(QueryString q, err) ->
              case err of
                InvalidQuery e -> do
                  putStr $ clrRed "Invalid query: " ++ q
                  putStrLn $ clrYellow "Reason: " ++ e
                UnknownQueryError e -> putStrLn $ clrRed "Unknown query error: " ++ q ++ "\n" ++ e
                QueryTimeout -> putStrLn $ clrRed "Query timed out: " ++ q
          )
          errs
        pure $ f : errFs
      ParseError e -> do
        putStrLn $ clrRed "Parsing error: " ++ e
        pure $ f : errFs

runQueriesFromFile :: Pipeling -> FilePath -> AppM FileTestResult
runQueriesFromFile pipeling file = ExceptT $ do
  queries <- runAppM $ extractQueriesFromFile pipeling file
  case queries of
    Right qs -> do
      qres <-
        mapConcurrently
          ( \q -> do
              r <- runQuery q
              pure (q, r)
          )
          qs
      let onlyErrs = filter (\(_, res) -> not (isRight res)) qres
      if null onlyErrs
        then pure . Right $ NoErrors
        else
          pure
            . Right
            . QueryExecutionErrors
            . reverse
            . foldl'
              ( \acc (q, res) ->
                  case res of
                    Left err -> (q, err) : acc
                    _ -> acc
              )
              []
            $ onlyErrs
    Left (QueryExtractionError err) -> pure . Right $ ParseError err
    Left err -> pure . Left $ err

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
  QueryExtractionError msg -> clrRed "Error when trying to extract queries: " ++ msg
  ExecError e -> clrRed "Error executing command: " ++ e
  TimeoutError cmd -> clrRed "Command timed out:" ++ cmd
  InvalidPath e -> clrRed "Invalid path: " ++ e
  UnknownError e -> clrRed "Error:" ++ e

getVersion :: String
getVersion = "v" ++ showVersion version