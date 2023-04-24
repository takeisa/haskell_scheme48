{-# LANGUAGE PackageImports #-}

module Main (main) where
import System.Environment (getArgs)
import Parser (parse)
import Lisp (LispVal(LvString))
import Eval (eval)

-- import "scheme48" Lib

main :: IO ()
-- main = Lib.someFunc
main = do
  getArgs >>= print .eval . readExpr . head

readExpr :: String -> LispVal
readExpr input =
  case parse input of
    Left err -> LvString $ "No match: " ++ show err
    Right val -> val

