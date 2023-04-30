module Eval (eval) where
import Lisp (LispVal (LvString, LvNumber, LvBool, LvList, LvAtom))
import Error (ThrowsError, LispError(..))
import Control.Monad.Except (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(LvString _) = return val
eval val@(LvNumber _) = return val
eval val@(LvBool _) = return val
eval (LvList [LvAtom "quote", val]) = return val
eval (LvList (LvAtom func: args)) = 
    mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotinent", numericBinOp quot),
              ("remainder", numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op singleValue@[_] = throwError $ NumArgs 2 singleValue
numericBinOp op args = mapM unpackNum args >>= return . LvNumber . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LvNumber n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum