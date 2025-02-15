{-# LANGUAGE OverloadedStrings #-}

module QueryExtractor where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.List (isPrefixOf)
import Types

sqlPrefix :: String
sqlPrefix = "```sql"

postgresPrefix :: String
postgresPrefix = "```sql+postgres"

eoqStart :: String -> Bool
eoqStart str = (==) (unwords . words $ str) "sql = <<-EOQ"

eoqEnd :: String -> Bool
eoqEnd str = (==) (unwords . words $ str) "EOQ" && not (eoqStart str)

extractQueryFromString :: Pipeling -> String -> Either AppError [QueryString]
extractQueryFromString pipeling str = fn (Right (False, [], [])) (zip [1 ..] $ lines str)
 where
  fn :: Either AppError (Bool, [String], [QueryString]) -> [(Int, String)] -> Either AppError [QueryString]
  fn (Left err) _ = Left err
  fn (Right (mode, _, qs)) []
    | mode = Left $ QueryExtractorError "Reached end of file, but query block seems unterminated."
    | otherwise = Right qs
  fn (Right (mode, acc, qs)) ((n, l) : ls)
    | not mode && isStartLine pipeling l = fn (Right (True, [], qs)) ls
    | not mode = fn (Right (False, [], qs)) ls
    | mode && isTerminateLine pipeling l = fn (Right (False, [], qs ++ [QueryString (unlines acc)])) ls
    | mode && isNormalLine pipeling l = fn (Right (mode, acc ++ [l], qs)) ls
    | mode && isInvalidLine pipeling l ls = Left $ QueryExtractorError $ "Unterminted query block near line " ++ show n ++ "."
    | otherwise = fn (Right (False, [], qs)) ls

  isStartLine :: Pipeling -> String -> Bool
  isStartLine Steampipe line = isPrefixOf postgresPrefix line || line == "```sql"
  isStartLine Powerpipe line = eoqStart line

  isTerminateLine :: Pipeling -> String -> Bool
  isTerminateLine Steampipe line = line == "```"
  isTerminateLine Powerpipe line = eoqEnd line

  isNormalLine :: Pipeling -> String -> Bool
  isNormalLine Steampipe line = line /= "```" && not (sqlPrefix `isPrefixOf` line) && not ("#" `isPrefixOf` line)
  isNormalLine Powerpipe line = not (eoqStart line) && not (eoqEnd line)

  isInvalidLine :: Pipeling -> String -> [(Int, String)] -> Bool
  isInvalidLine Steampipe line ls
    | "#" `isPrefixOf` line = True
    | sqlPrefix `isPrefixOf` line = True
    | null ls = True
    | otherwise = False
  isInvalidLine Powerpipe line ls
    | "#" `isPrefixOf` line = True
    | eoqStart line = True
    | null ls = True
    | otherwise = False

extractQueriesFromFile :: Pipeling -> FilePath -> AppM [QueryString]
extractQueriesFromFile pipeling path = ExceptT $ do
  content <- readFile path
  return $ case extractQueryFromString pipeling content of
    Left (QueryExtractorError err) -> Left [QueryExtractorError err]
    Left err -> Left [UnknownError (show err)]
    Right queries -> return queries
