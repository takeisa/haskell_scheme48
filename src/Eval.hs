module Eval (eval) where
import Lisp (LispVal (LvString, LvNumber, LvBool, LvList, LvAtom, LvDottedList))
import Error (ThrowsError, LispError(..))
import Control.Monad.Except (throwError, MonadError)
import Foreign.C (throwErrno)

eval :: LispVal -> ThrowsError LispVal
eval val@(LvString _) = return val
eval val@(LvNumber _) = return val
eval val@(LvBool _) = return val
eval (LvList [LvAtom "quote", val]) = return val
eval (LvList [LvAtom "if", pred, conseq, alt]) =
    do 
        result <- eval pred
        case result of
            LvBool False -> eval alt
            otherwise -> eval conseq
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
              ("string>=?", stringBoolBinOp (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("equal?", equal)]

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

car :: [LispVal] -> ThrowsError LispVal
car [LvList (x : _xs)] = return x
car [LvDottedList (x : _xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LvList (_ : xs)] = return $ LvList xs
cdr [LvDottedList _xs last] = return $ last
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, LvList []] = return $ LvList [x]
cons [x, LvList xs] = return $ LvList (x : xs)
cons [x, LvDottedList xs last] = return $ LvDottedList (x : xs) last
cons [x1, x2] = return $ LvDottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

{- TODO To implement eq?, eqv? add id to the LispVal data type. -}
equal :: [LispVal] -> ThrowsError LispVal
equal [(LvBool value1), (LvBool value2)] = return $ LvBool $ value1 == value2
equal [(LvNumber value1), (LvNumber value2)] = return $ LvBool $ value1 == value2
equal [(LvAtom value1), (LvAtom value2)] = return $ LvBool $ value1 == value2
equal [(LvString string1), (LvString string2)] = return $ LvBool $ string1 == string2
equal [(LvList list1), (LvList list2)] = return $ LvBool $ 
        (length list1 == length list2) && (all equalPair $ zip list1 list2)
    where equalPair (x1, x2) = case equal [x1, x2] of
                                    Left _err -> False
                                    Right (LvBool value) -> value

equal [_, _] = return $ LvBool False
equal badArgList = throwError $ NumArgs 2 badArgList