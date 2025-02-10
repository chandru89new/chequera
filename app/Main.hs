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
import Exec (clrBlue, clrGreen, clrRed, clrYellow)
import QueryExtractor
import Steampipe
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Types

main :: IO ()
main = do
  args <- getArgs
  files <- findAllDocFiles (head args)
  (eCount, sCount, tCount) <-
    foldM
      ( \(errorCount :: Int, successCount :: Int, totalCount :: Int) f -> do
          putStrLn $ clrBlue "Testing: " ++ f
          res <- testFile f
          case res of
            Left errs -> do
              mapM_ putStrLn errs
              pure (errorCount + length errs, successCount, totalCount + 1)
            Right _ -> do
              putStrLn $ clrGreen "All good!" ++ "\n"
              pure (errorCount, successCount + 1, totalCount + 1)
      )
      (0, 0, 0)
      files
  putStrLn $ clrRed "Errors: " ++ show eCount
  putStrLn $ clrGreen "Successes: " ++ show sCount
  putStrLn $ "Total: " ++ show tCount

runQueriesFromFile :: FilePath -> IO (Either [AppError] ())
runQueriesFromFile file = do
  queries <- try $ extractQueriesFromFile file
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

findAllDocFiles :: FilePath -> IO [FilePath]
findAllDocFiles dir = do
  ignoresString <- lookupEnv "IGNORE"
  let ignores = case ignoresString of
        Nothing -> []
        Just s -> map unpack $ splitOn (pack ",") $ pack s
  unless (null ignores) $ putStrLn $ clrYellow "Ignoring: " ++ show ignores
  res <- readProcessWithExitCode "find" [dir, "-type", "f", "-name", "*.md"] ""
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
  QueryExtractorError msg -> clrRed "Error when trying to extract queries: " ++ msg ++ "\n"
  QueryExecError (QueryString q, e) -> clrRed "Error in query: " ++ q ++ "\n" ++ "Reason: " ++ e ++ "\n"
  ExecError e -> clrRed "Error in shell command: " ++ e ++ "\n"
  TimeoutError query -> clrRed "Timeout in query: " ++ query ++ "\n"
  UnknownError -> clrRed "Some weird shit happened." ++ "\n"

testFile :: FilePath -> IO (Either [String] ())
testFile file = do
  res <- runQueriesFromFile file
  return $ case res of
    Left errs -> Left $ map prettyPrintError errs
    Right _ -> Right ()