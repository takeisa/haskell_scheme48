module Eval (eval) where
import Lisp (LispVal (LvString, LvNumber, LvBool, LvList, LvAtom))
import Error (ThrowsError, LispError(..))
import Control.Monad.Except (throwError, MonadError)
import Foreign.C (throwErrno)

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
              ("remainder", numericBinOp rem),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              -- TODO Functions not defined in Scheme
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", stringBoolBinOp (==)),
              ("string<?", stringBoolBinOp (<)),
              ("string>?", stringBoolBinOp (>)),
              ("string<=?", stringBoolBinOp (<=)),
              ("string>=?", stringBoolBinOp (>=))]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _op singleValue@[_] = throwError $ NumArgs 2 singleValue
numericBinOp op args = mapM unpackNum args >>= return . LvNumber . foldl1 op

{- boolBinOp :: MonadError LispError m =>
  (LispVal -> m t) -> (t -> t -> Bool) -> [LispVal] -> m LispVal
-}
 
boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = 
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ LvBool $ left `op` right

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

stringBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
stringBoolBinOp = boolBinOp unpackString

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LvNumber n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LvBool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean"  notBool

unpackString :: LispVal -> ThrowsError String
unpackString (LvString s) = return s
unpackString notString = throwError $ TypeMismatch "string" notString
