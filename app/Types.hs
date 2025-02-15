module Types where

import Control.Exception (Exception)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

data AppError
  = QueryExtractorError String
  | QueryExecError (QueryString, String)
  | ExecError String
  | TimeoutError String
  | UnknownError String
  deriving (Show)

instance Exception AppError

newtype QueryString = QueryString String deriving (Show)

data Command
  = Invalid
  | Help
  | Test Pipeling String
  | Version

data Pipeling = Steampipe | Powerpipe deriving (Eq, Show)

type AppM' e a = ExceptT e IO a
type AppM a = AppM' [AppError] a

runAppM :: AppM a -> IO (Either [AppError] a)
runAppM = runExceptT

data TestExitState = TestExitSuccess | TestExitFailure deriving (Show)