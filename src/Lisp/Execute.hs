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
eval val@(StringElement _) = return val
eval val@(IntElement _) = return val
eval val@(FloatElement _) = return val
eval val@(BoolElement _) = return val
eval (QuotedElement e) = return e
eval v@(VectorElement _) = return v
eval (ListElement [AtomElement "if", condition, true, false]) = do
  result <- eval condition
  eval $ case result of
    BoolElement True -> true
    _ -> false
eval (ListElement (AtomElement func : args)) = mapM eval args >>= apply func -- function application

apply :: String -> [Element] -> ThrowsError Element
apply func args =
  maybe
    (throwError $ UnboundVar "Unknown function " func)
    ($ args)
    (Map.lookup func primitiveFunctions)

coerceToNum :: Element -> ThrowsError Double
coerceToNum (IntElement i) = return $ fromInteger i
coerceToNum (FloatElement f) = return f
coerceToNum (StringElement s) =
  let parsed = reads s
   in if null parsed
        then throwError $ TypeMismatch "number" $ StringElement s
        else return $ fst $ head parsed
coerceToNum (ListElement [a]) = coerceToNum a
coerceToNum illegal = throwError $ TypeMismatch "number" illegal

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

numBinaryOp :: (Double -> Double -> Double) -> [Element] -> ThrowsError Element
numBinaryOp _ [] = throwError $ InvalidArgCount 2 []
numBinaryOp _ single@[_] = throwError $ InvalidArgCount 2 single
numBinaryOp op params = do
  let toAST e = if isInt e then IntElement $ floor e else FloatElement e
  mapM coerceToNum params <&> (toAST . foldl1 op)

compareElements :: [Element] -> ThrowsError Element
compareElements [a, b] = return $ BoolElement $ compare2Elements a b
compareElements elements = throwError $ InvalidArgCount 2 elements

compare2Elements :: Element -> Element -> Bool
compare2Elements (StringElement s1) (StringElement s2) = s1 == s2
compare2Elements (IntElement s1) (IntElement s2) = s1 == s2
compare2Elements (FloatElement s1) (FloatElement s2) = s1 == s2
compare2Elements (AtomElement a1) (AtomElement a2) = a1 == a2
compare2Elements (BoolElement b1) (BoolElement b2) = b1 == b2
compare2Elements (ListElement l1) (ListElement l2) = all (uncurry compare2Elements) $ zip l1 l2
compare2Elements (VectorElement l1) (VectorElement l2) = all (uncurry compare2Elements) $ zip l1 l2
compare2Elements (StringElement s) e2 = show e2 == s
compare2Elements e1 (StringElement s) = show e1 == s
compare2Elements _ _ = False

primitiveFunctions :: Map String ([Element] -> ThrowsError Element)
primitiveFunctions =
  Map.fromList
    [ ("+", numBinaryOp (+)),
      ("-", numBinaryOp (-)),
      ("/", numBinaryOp (/)),
      ("*", numBinaryOp (*)),
      ("^", numBinaryOp (**)),
      ("=", compareElements)
    ]
