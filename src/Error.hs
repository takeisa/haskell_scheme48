module Error (
    LispError(..),
    ThrowsError, trapError, extractValue
    ) where
import Lisp (LispVal, showElements)
import Control.Monad.Except (catchError)
import Text.Parsec (ParseError)

data LispError =
    TypeMismatch String LispVal
    | ExprParseError ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | NumArgs Integer [LispVal]

showError :: LispError -> String
showError (TypeMismatch expected found) = 
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (ExprParseError parseError) =
    "Parse error at " ++ show parseError
showError (BadSpecialForm message form) =
    message ++ ": " ++ show form
showError (NotFunction message func) =
    message ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected
    ++ " args; found values " ++ showElements found

instance Show LispError where
    show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val