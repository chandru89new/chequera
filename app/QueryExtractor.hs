{-# LANGUAGE OverloadedStrings #-}

module QueryExtractor where

import Control.Exception (throwIO)
import Data.List (isPrefixOf)
import Types

sqlPrefix :: String
sqlPrefix = "```sql"

postgresPrefix :: String
postgresPrefix = "```sql+postgres"

extractQueryFromString :: String -> Either AppError [QueryString]
extractQueryFromString str = fn (Right (False, [], [])) (zip [1 ..] $ lines str)
 where
  fn :: Either AppError (Bool, [String], [QueryString]) -> [(Int, String)] -> Either AppError [QueryString]
  fn (Left err) _ = Left err
  fn (Right (mode, _, qs)) []
    | mode = Left $ QueryExtractorError "Reached end of file, but query block seems unterminated."
    | otherwise = Right qs
  fn (Right (mode, acc, qs)) ((n, l) : ls)
    | mode && isNormalLine l = fn (Right (mode, acc ++ [l], qs)) ls
    | mode && isTerminateLine l = fn (Right (False, [], qs ++ [QueryString ((unwords . words . unlines) acc)])) ls
    | mode && isInvalidLine l ls = Left $ QueryExtractorError $ "Unterminted query block near line " ++ show n ++ "."
    | not mode && isStartLine l = fn (Right (True, [], qs)) ls
    | otherwise = fn (Right (False, [], qs)) ls

  isStartLine :: String -> Bool
  isStartLine line = isPrefixOf postgresPrefix line || line == "```sql"

  isTerminateLine :: String -> Bool
  isTerminateLine line = line == "```"

  isNormalLine :: String -> Bool
  isNormalLine line = line /= "```" && not (sqlPrefix `isPrefixOf` line)

  isInvalidLine :: String -> [(Int, String)] -> Bool
  isInvalidLine line ls
    | sqlPrefix `isPrefixOf` line = True
    | null ls = True
    | otherwise = False

extractQueriesFromFile :: FilePath -> IO [QueryString]
extractQueriesFromFile path = do
  content <- readFile path
  case extractQueryFromString content of
    Left (QueryExtractorError err) -> throwIO (QueryExtractorError err)
    Left _ -> throwIO UnknownError
    Right queries -> return queries
