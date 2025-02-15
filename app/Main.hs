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
          _ <- putStrLn $ intercalate "\n" $ map prettyPrintError err
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
  liftIO $ putStrLn "Starting Steampipe service..."
  stopService
  startService
  liftIO $ putStrLn "Gathering the list of files..."
  (fs, excludes) <- findAllDocFiles Steampipe path
  unless (null excludes) $ liftIO $ putStrLn $ clrYellow "Ignoring these files because of ignore flag/pattern: " ++ intercalate ", " excludes
  (errCount, errFiles) <- liftIO $ foldM fn (0, []) fs
  liftIO $ unless (errCount == 0) $ putStrLn $ clrRed "\nErrors: " ++ show errCount
  liftIO $ putStrLn $ "Total files checked: " ++ show (length fs)
  liftIO $ unless (null errFiles) $ do
    putStrLn $ clrRed "There are errors in these files:"
    mapM_ putStrLn errFiles
  stopService
  if null errFiles
    then return TestExitSuccess
    else return TestExitFailure
 where
  fn :: (Int, [FilePath]) -> FilePath -> IO (Int, [FilePath])
  fn (eCount, eFiles) f = do
    putStrLn $ "\n" ++ clrBlue "Testing: " ++ f
    res <- runAppM $ runQueriesFromFile Steampipe f
    case res of
      Left errs -> do
        _ <- putStrLn $ intercalate "\n" $ map prettyPrintError errs
        pure (eCount + length errs, f : eFiles)
      Right _ -> do
        putStrLn $ clrGreen "All good!"
        pure (eCount, eFiles)

runQueriesFromFile :: Pipeling -> FilePath -> AppM ()
runQueriesFromFile pipeling file = do
  queries <- extractQueriesFromFile pipeling file
  ExceptT $ concatErrors $ mapConcurrently (runAppM . runQ) queries
 where
  concatErrors :: IO [Either [AppError] ()] -> IO (Either [AppError] ())
  concatErrors ios = do
    res <- ios
    foldM fn (Right ()) res
   where
    fn :: Either [AppError] () -> Either [AppError] () -> IO (Either [AppError] ())
    fn (Left errs) (Left errs') = pure $ Left $ errs ++ errs'
    fn (Left errs) _ = pure $ Left errs
    fn _ (Left errs) = pure $ Left errs
    fn _ _ = pure $ Right ()

  runQ :: QueryString -> AppM ()
  runQ (QueryString q) = runQuery q

findAllDocFiles :: Pipeling -> FilePath -> AppM ([FilePath], [FilePath])
findAllDocFiles pipeling dir = do
  ignoresString <- liftIO $ lookupEnv "CQ_IGNORE"
  let ignores = case ignoresString of
        Nothing -> []
        Just s -> map unpack $ splitOn (pack ",") $ pack s
  res <- liftIO $ readProcessWithExitCode "find" [dir, "-type", "f", "-name", if pipeling == Steampipe then "*.md" else "*.pp"] ""
  ExceptT $ return $ case res of
    (ExitSuccess, output, _) ->
      let files = filter (not . patternInIgnoreList ignores) $ sort $ lines output
          excludeFiles = filter (patternInIgnoreList ignores) $ sort $ lines output
       in Right (files, excludeFiles)
    (ExitFailure _, _, err) -> Left [QueryExtractorError err]
 where
  patternInIgnoreList :: [String] -> String -> Bool
  patternInIgnoreList ignorePatterns fileName = any (`isInfixOf` fileName) ignorePatterns

prettyPrintError :: AppError -> String
prettyPrintError err = case err of
  QueryExtractorError msg -> clrRed "Error when trying to extract queries: " ++ msg
  QueryExecError (QueryString q, e) -> clrRed "Error in SQL query: " ++ q ++ "Reason: " ++ e
  ExecError e -> clrRed "Error executing command: " ++ e
  TimeoutError query -> clrRed "Timeout in query: " ++ query
  UnknownError e -> clrRed "Error:" ++ e

getVersion :: String
getVersion = "v" ++ showVersion version