module Exec where

import Control.Exception (throwIO)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Types (AppError (..))

defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 30 * 1000000

-- exec' discards a successful result, and throws if there is any error
exec' :: String -> [String] -> IO ()
exec' cmd opts = do
  timeoutSeconds <- do
    t <- lookupEnv "CQ_TIMEOUT"
    case t of
      Nothing -> return defaultTimeoutSeconds
      Just s -> return $ maybe defaultTimeoutSeconds (* 1000000) $ readMaybe s
  res <- timeout timeoutSeconds $ readProcessWithExitCode cmd opts ""
  case res of
    Just (ExitSuccess, _, _) -> return ()
    Just (ExitFailure _, _, err) -> throwIO (ExecError err)
    Nothing -> throwIO $ TimeoutError (unwords (cmd : opts))

clrRed :: String -> String
clrRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

clrGreen :: String -> String
clrGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

clrBlue :: String -> String
clrBlue s = "\x1b[34m" ++ s ++ "\x1b[0m"

clrYellow :: String -> String
clrYellow s = "\x1b[33m" ++ s ++ "\x1b[0m"