module Eval (eval) where
import Lisp (LispVal (LvString, LvNumber, LvBool, LvList, LvAtom))

eval :: LispVal -> LispVal
eval val@(LvString _) = val
eval val@(LvNumber _) = val
eval val@(LvBool _) = val
eval (LvList [LvAtom "quote", val]) = val
eval (LvList (LvAtom func: args)) =
    apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply func args = maybe (LvBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = LvNumber $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (LvNumber n) = n
-- TODO error
unpackNum _ = 0