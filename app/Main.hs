{-# LANGUAGE PackageImports #-}

module Main (main) where
import System.Environment (getArgs)
import Parser (parse)
import Eval (eval)
import Control.Monad.Except (throwError, liftM)
import Error (ThrowsError, LispError(ExprParseError), extractValue, trapError)
import Lisp (LispVal)
-- import "scheme48" Lib

main :: IO ()
-- main = Lib.someFunc
main = do
  args <- getArgs
  let evaled = liftM show (readExpr (head args) >>= eval)
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse input of
    Left err -> throwError $ ExprParseError err
    Right val -> return val

