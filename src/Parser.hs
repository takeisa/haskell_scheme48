module Parser (parse) where
import Text.ParserCombinators.Parsec (
    Parser, oneOf, skipMany1, space, char,
    many, noneOf, letter, (<|>), digit, many1) 
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Lisp (LispVal (LvString, LvBool, LvAtom, LvNumber))
import Control.Monad ( liftM )
import qualified Text.Parsec.Error

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces2 :: Parser ()
spaces2 = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ LvString x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> LvBool True
    "#f" -> LvBool False
    _ -> LvAtom atom

parseNumber :: Parser LispVal
{- parseNumber = do
  number <- many1 digit
  return $ Number $ read number
 -}
 -- TODO fmap better?
parseNumber = liftM (LvNumber . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- TODO Don't use ParseError
parse :: String -> Either Text.Parsec.Error.ParseError LispVal
parse = Parsec.parse parseExpr "lisp"