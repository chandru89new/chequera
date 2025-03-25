{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module QueryParser where

import Data.List (isInfixOf)

newtype Parser a = Parser (String -> Either Error (a, String))
type Error = String

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (a, rest) <- p input
    Right (f a, rest)

instance Applicative Parser where
  pure val = Parser $ \input -> Right (val, input)

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Left err -> Left err
    Right (f, rest) -> parse (fmap f (Parser p2)) rest

instance Monad Parser where
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = Parser $ \input -> case p input of
    Left err -> Left err
    Right (a, rest) -> do
      let (Parser newParser) = f a
      newParser rest

parse :: Parser a -> String -> Either String (a, String)
parse (Parser p) = p

str :: String -> Parser String
str str2check = Parser $ \input -> do
  let inputPart = take (length str2check) input
   in if inputPart == str2check
        then Right (str2check, drop (length str2check) input)
        else Left $ "String does not start with '" ++ str2check ++ "'."

skipTill :: String -> Parser String
skipTill string = Parser $ \input -> do
  case parse (str string) input of
    Right (match, rest) -> Right (match, rest)
    Left err -> case input of
      [] -> Left err
      (_ : cs) -> parse (skipTill string) cs

keepTill :: String -> Parser String
keepTill string = Parser $ \input -> go [] input
 where
  go :: String -> String -> Either String (String, String)
  go acc input = case parse (str string) input of
    Right (match, rest) -> Right (reverse acc, match ++ rest)
    Left err -> case input of
      [] -> Left err
      (c : cs) -> go (c : acc) cs

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Parser p1) (Parser p2) = Parser $ \input ->
  case p1 input of
    Right result -> Right result
    Left _ -> p2 input

mapError :: (String -> String) -> Parser a -> Parser a
mapError f p = Parser $ \input -> case parse p input of
  Left err -> Left $ f err
  Right x -> Right x

simpleSqlBlockParser :: Parser String
simpleSqlBlockParser = do
  _ <- mapError (const "No SQL block found.") $ skipTill "```sql\n"
  content <- mapError (const "No SQL block found.") $ keepTill "```\n"
  Parser $ \rest -> case parse (notContains "\n```sql") content of
    Left _ -> Left "Invalid SQL block."
    Right x -> Right (fst x, rest)

collectAll :: Parser a -> Parser [a]
collectAll (Parser p) = Parser $ \input -> go [] input
 where
  -- go :: [a] -> String -> Parser [a]
  go acc input =
    case p input of
      Right (matched, rest) -> go (matched : acc) rest
      Left err -> case (input, acc) of
        ([], []) -> Left err
        (_, []) -> Left err
        ([], acc') -> Right (reverse acc', input)
        (something, acc') -> Right (reverse acc', something)

postgresBlockParser :: Parser String
postgresBlockParser = do
  _ <- mapError (const "No SQL block found.") $ skipTill "```sql+postgres\n"
  content <- mapError (const "No SQL block found.") $ keepTill "```\n"
  Parser $ \rest -> case parse (notContains "```sql") content of
    Left _ -> Left "Invalid SQL block."
    Right x -> Right (fst x, rest)

notContains :: String -> Parser String
notContains string = Parser $ \input -> do
  if string `isInfixOf` input
    then Left $ "Input contains '" ++ string ++ "'."
    else Right (input, "")

test1 = parse (str "```sql\n") "before```sql\nsqlblock\nsql\nfjaslkdfjaklsdfj"
test1err = parse (str "```sql\n") "before```ql\nsqlblock\nsql\nfjaslkdfjaklsdfj"
test2 = parse (skipTill "sql") "before sql after"
test2err = parse (skipTill "sql") "before dsl after"
test2' = parse (skipTill "sql") "before dsl aftersql"
test3 = parse (keepTill "endsql") "before sql before endsql after after"
test4 = parse simpleSqlBlockParser "before before ```sql\n block block ` block ` ```\n after after after"
test4err = parse simpleSqlBlockParser "before before ```sql\n block block ```sql\nblock ```\n after after after"
test5 = parse (collectAll simpleSqlBlockParser) "jlkasjdfalsdjf ```sql\n sqlblock ```\n fjaksdlfjalsdf ```sql\n anothersqlblock ```\n fasdfasdf"
test5err = parse (collectAll simpleSqlBlockParser) "jlkasjdfalsdjf ```sql\n sqlblock ```\n fjaksdlfjalsdf ```sql\n anothersqlblock ```\n fasdfasdf"
test6 = parse postgresBlockParser "before before ```sql+postgres\n inner block ```\n fajsdklfajsdf"
test6err = parse postgresBlockParser "before before ```sql+postgres\n inner ```sqlblock ```\n fajsdklfajsdf"
test7 = parse (collectAll (postgresBlockParser <|> simpleSqlBlockParser)) "jlkasjdfalsdjf ```sql+postgres\n sqlblock ```\n fjaksdlfjalsdf ```sql\n anothersqlblock ```\n fasdfasdf"
test8 = parse (notContains "```sql") "```\nabcdjfksdf```sql\n"