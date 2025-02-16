module Types where

import Control.Exception (Exception)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

data AppError
  = ExecError String
  | TimeoutError String
  | UnknownError String
  | QueryExtractionError String
  | InvalidPath String
  deriving (Show)

data QueryExecError'
  = QueryTimeout
  | InvalidQuery String
  | UnknownQueryError String
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
type AppM a = AppM' AppError a

runAppM :: AppM a -> IO (Either AppError a)
runAppM = runExceptT

data TestExitState = TestExitSuccess | TestExitFailure deriving (Show)

data FileTestResult
  = ParseError String
  | QueryExecutionErrors [(QueryString, QueryExecError')]
  | NoErrors
  deriving (Show)