module Lisp.Errors where
import Text.ParserCombinators.Parsec (ParseError)
import Lisp.AST
import Control.Monad.Except

data LispError =
  UnboundVar String String
  | ParseError ParseError
  | TypeMismatch String Element
  | InvalidArgCount Int [Element]

showError :: LispError -> String
showError (UnboundVar message name) = message ++ ": " ++ name
showError (ParseError parseError) = "Parse error at " ++ show parseError
showError (TypeMismatch expected found) = "Type Mismatch: expected " ++ expected ++ " but found " ++ show found
showError (InvalidArgCount expected elems) = "Invalid arg count, expected " ++ show expected ++ " (or more) args but only found " ++ show elems

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)
