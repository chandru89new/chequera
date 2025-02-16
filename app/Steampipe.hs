{-# LANGUAGE ScopedTypeVariables #-}

module Steampipe where

import Exec (exec')
import Types (AppError (..), AppM, QueryExecError' (..), QueryString (..), runAppM)

startService :: AppM ()
startService = exec' "steampipe" (words "service start")

runQuery :: QueryString -> IO (Either QueryExecError' ())
runQuery (QueryString query) = do
  res <- runAppM $ exec' "steampipe" ["query", query]
  pure $ case res of
    Left (ExecError e) -> Left $ InvalidQuery e
    Left (TimeoutError _) -> Left QueryTimeout
    Left e -> Left $ UnknownQueryError (show e)
    _ -> Right ()

stopService :: AppM ()
stopService = exec' "steampipe" $ words "service stop"
