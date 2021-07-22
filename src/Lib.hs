module Lib where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import Control.Monad.Except
import Errors
import AST

symbol :: Parser Char
symbol = oneOf "?!#$&|+-/*^@-_<=>" -- symbols that can be used in atoms

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser Element
parseString = do
  _    <- char '"'
  body <- many (noneOf "\"")
  _    <- char '"'
  return $ StringElement body

parseAtom :: Parser Element
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $ case atom of
    "true" -> BoolElement True
    "false" -> BoolElement False
    _ ->  AtomElement atom

parseNumber :: Parser Element
parseNumber = do
  nums <- many1 (digit <|> char '.')
  if '.' `elem` nums
    then return $ FloatElement $ read nums
    else return $ IntElement $ read nums

parseList :: Parser Element
parseList = do
  _ <- char '('
  x <- ListElement <$> parseExpr `sepBy` spaces
  _ <- char ')'
  return x

parseVec :: Parser Element
parseVec = do
  _ <- char '['
  x <- VectorElement <$> sepBy parseExpr spaces
  _ <- char ']'
  return x

parseQuoted :: Parser Element
parseQuoted = do
  _ <- char '\''
  QuotedElement <$> parseExpr

--parseBool :: Parser Element
--parseBool = BoolElement $ (const True) <$> string "true" <|> const False <$> string "false"

parseExpr :: Parser Element
parseExpr =
  parseString
    <|> parseAtom
    <|> parseNumber
    <|> parseList
    <|> parseVec
    <|> parseQuoted

readExpr :: String -> ThrowsError Element
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ ParseError err
  Right val -> return val
