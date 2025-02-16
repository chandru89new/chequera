module Exec where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Types (AppError (..), AppM)

defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 30 * 1000000

-- exec' discards a successful result, and throws if there is any error
exec' :: String -> [String] -> AppM ()
exec' cmd opts = ExceptT $ do
  timeoutSeconds <- do
    t <- lookupEnv "CQ_TIMEOUT"
    case t of
      Nothing -> return defaultTimeoutSeconds
      Just s -> return $ maybe defaultTimeoutSeconds (* 1000000) $ readMaybe s
  res <- timeout timeoutSeconds $ readProcessWithExitCode cmd opts ""
  return $ case res of
    Just (ExitSuccess, _, _) -> Right ()
    Just (ExitFailure _, _, err) -> Left $ ExecError err
    Nothing -> Left $ TimeoutError (unwords (cmd : opts))

clrRed :: String -> String
clrRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

clrGreen :: String -> String
clrGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

clrBlue :: String -> String
clrBlue s = "\x1b[34m" ++ s ++ "\x1b[0m"

clrYellow :: String -> String
clrYellow s = "\x1b[33m" ++ s ++ "\x1b[0m"

logError :: [Char] -> [Char]
logError str = clrRed "[ERROR]: " ++ str
logWarning :: [Char] -> [Char]
logWarning str = clrYellow "[WARNING]: " ++ str
logSuccess :: [Char] -> [Char]
logSuccess str = clrGreen "[SUCCESS]: " ++ str
logInfo :: [Char] -> [Char]
logInfo str = clrBlue "[INFO]: " ++ str