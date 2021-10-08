module Lisp.Execute where

import Control.Monad.Except
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Lisp.AST
import Lisp.Environment
import Lisp.Errors

extractValue :: ThrowsError a -> a
extractValue (Right v) = v
extractValue (Left e) = error $ show e

eval :: Env -> Element -> IOThrowsError Element
eval env val@(String _) = return val
eval env val@(Int _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Quote e) = return e
eval env v@(Vector _) = return v
eval env (Atom name) = getVar env name
eval env (List [Atom "set!", Atom var, val]) = eval env val >>= setVar env var
eval env (List [Atom "def", Atom var, val]) = eval env val >>= defineVar env var
eval env (List [Atom "if", condition, true, false]) = do
  result <- eval env condition
  eval env $ case result of
    Bool True -> true
    _ -> false
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func -- function application

apply :: String -> [Element] -> ThrowsError Element
apply func args =
  maybe
    (throwError $ UnboundVar "Unknown function " func)
    ($ args)
    (Map.lookup func primitiveFunctions)

coerceToNum :: Element -> ThrowsError Double
coerceToNum (Int i) = return $ fromInteger i
coerceToNum (Float f) = return f
coerceToNum (String s) =
  let parsed = reads s
   in if null parsed
        then throwError $ TypeMismatch "number" $ String s
        else return $ fst $ head parsed
coerceToNum (List [a]) = coerceToNum a
coerceToNum illegal = throwError $ TypeMismatch "number" illegal

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

numBinaryOp :: (Double -> Double -> Double) -> [Element] -> ThrowsError Element
numBinaryOp _ [] = throwError $ InvalidArgCount 2 []
numBinaryOp _ single@[_] = throwError $ InvalidArgCount 2 single
numBinaryOp op params = do
  let toAST e = if isInt e then Int $ floor e else Float e
  mapM coerceToNum params <&> (toAST . foldl1 op)

compareElements :: [Element] -> ThrowsError Element
compareElements [a, b] = return $ Bool $ compare2Elements a b
compareElements elements = throwError $ InvalidArgCount 2 elements

compare2Elements :: Element -> Element -> Bool
compare2Elements (String s1) (String s2) = s1 == s2
compare2Elements (Int s1) (Int s2) = s1 == s2
compare2Elements (Float s1) (Float s2) = s1 == s2
compare2Elements (Atom a1) (Atom a2) = a1 == a2
compare2Elements (Bool b1) (Bool b2) = b1 == b2
compare2Elements (List l1) (List l2) = all (uncurry compare2Elements) $ zip l1 l2
compare2Elements (Vector l1) (Vector l2) = all (uncurry compare2Elements) $ zip l1 l2
compare2Elements (String s) e2 = show e2 == s
compare2Elements e1 (String s) = show e1 == s
compare2Elements _ _ = False

headElem :: [Element] -> ThrowsError Element
headElem [List (a : _)] = return a
headElem [List []] = throwError $ RuntimeError "List is empty"
headElem [Vector (a : _)] = return a
headElem [Vector []] = throwError $ RuntimeError "List is empty"
headElem badArgList = throwError $ InvalidArgCount 1 badArgList

tailElem :: [Element] -> ThrowsError Element
tailElem [List (_ : xs)] = return $ List xs
tailElem [List []] = throwError $ RuntimeError "List is empty"
tailElem [Vector (_ : xs)] = return $ Vector xs
tailElem [Vector []] = throwError $ RuntimeError "List is empty"
tailElem badArgList = throwError $ InvalidArgCount 1 badArgList

cons :: [Element] -> ThrowsError Element
cons [e, List []] = return $ List [e]
cons [e, Vector []] = return $ Vector [e]
cons [e, List l] = return $ List (e : l)
cons [e, Vector l] = return $ Vector (e : l)
cons badArgList = throwError $ InvalidArgCount 2 badArgList

primitiveFunctions :: Map String ([Element] -> ThrowsError Element)
primitiveFunctions =
  Map.fromList
    [ ("+", numBinaryOp (+)),
      ("-", numBinaryOp (-)),
      ("/", numBinaryOp (/)),
      ("*", numBinaryOp (*)),
      ("^", numBinaryOp (**)),
      ("=", compareElements),
      ("head", headElem),
      ("tail", tailElem),
      ("cons", cons)
    ]
