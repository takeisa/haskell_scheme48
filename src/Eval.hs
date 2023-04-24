module Eval (eval) where
import Lisp (LispVal (LvString, LvNumber, LvBool, LvList, LvAtom))

eval :: LispVal -> LispVal
eval val@(LvString _) = val
eval val@(LvNumber _) = val
eval val@(LvBool _) = val
eval (LvList [LvAtom "quote", val]) = val