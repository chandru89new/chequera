{-# LANGUAGE ScopedTypeVariables #-}

module Steampipe where

import Control.Exception (try)
import Exec (exec')
import Types (AppError (..), QueryString (QueryString))

startService :: IO ()
startService = exec' "steampipe" (words "service start")

runQuery :: String -> IO (Either AppError ())
runQuery query = do
  res <- try $ exec' "steampipe" ["query", query]
  return $ case res of
    Left (err :: AppError) -> case err of
      ExecError e -> Left (QueryExecError (QueryString query, show e))
      TimeoutError _ -> Left $ TimeoutError query
      _ -> Left err
    Right _ -> Right ()

stopService :: IO ()
stopService = exec' "steampipe" $ words "service stop"

wrapInQuotes :: String -> String
wrapInQuotes s = "\"" ++ s ++ "\""