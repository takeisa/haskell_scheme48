module Lisp (
  LispVal(..), 
  showVal, showElements) where
import Prelude hiding (last)

data LispVal = 
  LvAtom String
  | LvList [LispVal]
  | LvDottedList [LispVal] LispVal
  | LvNumber Integer
  | LvString String
  | LvBool Bool

instance Show LispVal where 
  show = showVal

showVal :: LispVal -> String
showVal (LvAtom name) = name
showVal (LvNumber number) = show number
showVal (LvString contents) = contents
showVal (LvBool True) = "#t"
showVal (LvBool False) = "#f"
showVal (LvList elements) = "(" ++ showElements elements ++ ")"
showVal (LvDottedList butlast last) = 
  "(" ++ showElements butlast ++ " . " ++ showVal last ++ ")"

showElements :: [LispVal] -> String
showElements = unwords . map showVal