module Types where

import Control.Exception (Exception)

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
