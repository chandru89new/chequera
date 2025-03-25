module QueryExtractor where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified QueryParser as QP
import Types

extractQueriesFromFile :: Pipeling -> FilePath -> FileTestM [QueryString]
extractQueriesFromFile _ path = ExceptT $ do
  content <- readFile path
  pure $ case QP.parse (QP.collectAll (QP.simpleSqlBlockParser QP.<|> QP.postgresBlockParser)) content of
    Left err -> Left (ParseError' err)
    Right (qs, _) -> Right (map QueryString qs)

-- testExtractor filePath = do
--   runFileTestM $ extractQueriesFromFile Steampipe filePath

-- test filePath = do
--   contents <- readFile filePath
--   print $ QP.parse (QP.simpleSqlBlockParser QP.<|> QP.postgresBlockParser) contents