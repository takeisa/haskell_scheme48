{-# LANGUAGE PackageImports #-}

module Main (main) where
import System.Environment (getArgs)

import Text.ParserCombinators.Parsec (
  oneOf, Parser, parse, space, skipMany1, char, many,
  noneOf, letter, digit, (<|>), many1)
import Control.Monad (liftM)

-- import "scheme48" Lib

main :: IO ()
-- main = Lib.someFunc
main = do
  args <- getArgs
  putStrLn $ "hello, " ++ head args
  putStrLn $ readExpr $ head args

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces2 :: Parser ()
spaces2 = skipMany1 space

data LispVal = 
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
{- parseNumber = do
  number <- many1 digit
  return $ Number $ read number
 -}
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = 
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

