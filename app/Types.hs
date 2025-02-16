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

-- this is the overall monad wrapping the app
type AppM a = ExceptT AppError IO a

runAppM :: AppM a -> IO (Either AppError a)
runAppM = runExceptT

data TestExitState = TestExitSuccess | TestExitFailure deriving (Show)

-- this one here helps us run tests in a file and collect the results
type FileTestM a = ExceptT FileTestError IO a

runFileTestM :: FileTestM a -> IO (Either FileTestError a)
runFileTestM = runExceptT

data FileTestError
  = ParseError' String
  | QueryExecError [(QueryString, QueryExecError')]
  deriving (Show)

instance Exception FileTestError
