module Lisp.Execute where

import Control.Monad.Except
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Lisp.AST
import Lisp.Errors

newtype Stack a = Stack
  { _values :: [a]
  }
  deriving (Show, Eq)

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton a = Stack [a]

fromList :: [a] -> Stack a
fromList = Stack

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, empty)
pop (Stack (x : xs)) = (Just x, fromList xs)

push :: a -> Stack a -> Stack a
push e (Stack l) = Stack (e : l)

instance Functor Stack where
  fmap f (Stack l) = fromList $ fmap f l

instance Foldable Stack where
  foldMap f (Stack l) = foldMap f l
  foldr f b (Stack l) = foldr f b l

extractValue :: ThrowsError a -> a
extractValue (Right v) = v
extractValue (Left e) = error $ show e

eval :: Element -> ThrowsError Element
eval val@(String _) = return val
eval val@(Int _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (Quote e) = return e
eval v@(Vector _) = return v
eval (List [Atom "if", condition, true, false]) = do
  result <- eval condition
  eval $ case result of
    Bool True -> true
    _ -> false
eval (List (Atom func : args)) = mapM eval args >>= apply func -- function application

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

primitiveFunctions :: Map String ([Element] -> ThrowsError Element)
primitiveFunctions =
  Map.fromList
    [ ("+", numBinaryOp (+)),
      ("-", numBinaryOp (-)),
      ("/", numBinaryOp (/)),
      ("*", numBinaryOp (*)),
      ("^", numBinaryOp (**)),
      ("=", compareElements),
      ("head", headElem)
    ]
