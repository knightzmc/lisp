module Lisp.Errors where

import Control.Monad.Except
import Lisp.AST
import Text.ParserCombinators.Parsec (ParseError)

data LispError
  = UnboundVar String String
  | ParseError ParseError
  | TypeMismatch String Element
  | InvalidArgCount Int [Element]
  | RuntimeError String

showError :: LispError -> String
showError (UnboundVar message name) = message ++ ": " ++ name
showError (ParseError parseError) = "Parse error at " ++ show parseError
showError (TypeMismatch expected found) = "Type Mismatch: expected " ++ expected ++ " but found " ++ show found
showError (InvalidArgCount expected elems) = "Invalid arg count, expected " ++ show expected ++ " (or more) args but only found " ++ show elems
showError (RuntimeError message) = "Error: " ++ message

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
