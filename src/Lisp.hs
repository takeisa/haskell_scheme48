module Lisp (LispVal(LvAtom, LvList, LvDottedList, LvNumber, LvString, LvBool)) where

data LispVal = 
  LvAtom String
  | LvList [LispVal]
  | LvDottedList [LispVal] LispVal
  | LvNumber Integer
  | LvString String
  | LvBool Bool
  deriving (Show)
