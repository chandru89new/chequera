module Types where

import Control.Exception (Exception)

data AppError
  = QueryExtractorError String
  | QueryExecError (QueryString, String)
  | ExecError String
  | TimeoutError String
  | UnknownError
  deriving (Show)

instance Exception AppError

newtype QueryString = QueryString String deriving (Show)
