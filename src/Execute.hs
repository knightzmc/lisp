module Execute where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Lib

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

eval :: Element -> Element
eval val@(StringElement _                        ) = val
eval val@(IntElement    _                        ) = val
eval val@(FloatElement  _                        ) = val
eval (    QuotedElement e                        ) = e
eval (    ListElement   (AtomElement func : args)) = apply func $ map eval args

apply :: String -> [Element] -> Element
apply func args =
  maybe (StringElement "unknown function") ($ args)
    $ Map.lookup func primitiveFunctions

coerceToNum :: Element -> Double
coerceToNum (IntElement    i  ) = fromInteger i
coerceToNum (FloatElement  f  ) = f
coerceToNum (StringElement s  ) = read s
coerceToNum (ListElement   [a]) = coerceToNum a
coerceToNum _                   = 0

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

numBinaryOp :: (Double -> Double -> Double) -> [Element] -> Element
numBinaryOp op params = do
  let result = foldl1 op $ map coerceToNum params
  if isInt result then IntElement $ floor result else FloatElement result


primitiveFunctions :: Map String ([Element] -> Element)
primitiveFunctions = Map.fromList
  [ ("+", numBinaryOp (+))
  , ("-", numBinaryOp (-))
  , ("/", numBinaryOp (/))
  , ("*", numBinaryOp (*))
  , ("^", numBinaryOp (**))
  ]
