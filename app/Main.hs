{-# LANGUAGE PackageImports #-}

module Main (main) where
import System.Environment (getArgs)
import Parser (parse)

-- import "scheme48" Lib

main :: IO ()
-- main = Lib.someFunc
main = do
  args <- getArgs
  putStrLn $ "hello, " ++ head args
  putStrLn $ readExpr $ head args

readExpr :: String -> String
readExpr input = 
  case parse input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

