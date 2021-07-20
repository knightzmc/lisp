module Execute where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           AST
import Errors
import Control.Monad.Except
import Data.Functor
newtype Stack a =
  Stack
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
pop (Stack []      ) = (Nothing, empty)
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
eval val@(StringElement _                        ) = return val
eval val@(IntElement    _                        ) = return val
eval val@(FloatElement  _                        ) = return val
eval (    QuotedElement e                        ) = return e
eval v@(VectorElement _) = return v
eval (    ListElement   (AtomElement func : args)) = mapM eval args >>= apply func

apply :: String -> [Element] -> ThrowsError Element
apply func args =
  maybe (throwError $ UnboundVar "Unknown function " func)
        ($ args)
        (Map.lookup func primitiveFunctions)

coerceToNum :: Element -> ThrowsError Double
coerceToNum (IntElement    i  ) = return $ fromInteger i
coerceToNum (FloatElement  f  ) = return f
coerceToNum (StringElement s  ) = let parsed = reads s in
                                    if null parsed
                                       then throwError $ TypeMismatch "number" $ StringElement s
                                          else return $ fst $ head parsed
coerceToNum (ListElement   [a]) = coerceToNum a
coerceToNum illegal                   = throwError $ TypeMismatch "number" illegal

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

numBinaryOp :: (Double -> Double -> Double) -> [Element] -> ThrowsError Element
numBinaryOp _ [] = throwError $ InvalidArgCount 2 []
numBinaryOp _ single@[_] = throwError $ InvalidArgCount 2 single
numBinaryOp op params = do
  let toAST e = if isInt e then IntElement $ floor e else FloatElement e
  mapM coerceToNum params <&> (toAST . foldl1 op)


primitiveFunctions :: Map String ([Element] -> ThrowsError Element)
primitiveFunctions = Map.fromList
  [ ("+", numBinaryOp (+))
  , ("-", numBinaryOp (-))
  , ("/", numBinaryOp (/))
  , ("*", numBinaryOp (*))
  , ("^", numBinaryOp (**))
  ]
