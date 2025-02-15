{-# LANGUAGE ScopedTypeVariables #-}

module Steampipe where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Exec (exec')
import Types (AppError (..), AppM, QueryString (QueryString), runAppM)

startService :: AppM ()
startService = exec' "steampipe" (words "service start")

runQuery :: String -> AppM ()
runQuery query = ExceptT $ do
  res <- runAppM $ exec' "steampipe" ["query", query]
  pure $ case res of
    Left [ExecError e] -> Left [QueryExecError (QueryString query, e)]
    Left e -> Left e
    _ -> Right ()

stopService :: AppM ()
stopService = exec' "steampipe" $ words "service stop"
