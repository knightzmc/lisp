module Lib where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

data Element
  = AtomElement String
  | IntElement Integer
  | FloatElement Double
  | StringElement String
  | VectorElement [Element]
  | KeywordElement String
  | ListElement [Element]
  | QuotedElement Element
  deriving (Eq)

instance Show Element where
  show = showElement

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
  return $ AtomElement atom

parseNumber :: Parser Element
parseNumber = do
  nums <- many1 (digit <|> char '.')
  if '.' `elem` nums
    then return $ FloatElement $ read nums
    else return $ IntElement $ read nums

parseList :: Parser Element
parseList = do
  _ <- char '('
  x <- ListElement <$> sepBy parseExpr spaces
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
  x <- parseExpr
  return $ QuotedElement x

parseExpr :: Parser Element
parseExpr =
  parseString
    <|> parseAtom
    <|> parseNumber
    <|> parseList
    <|> parseVec
    <|> parseQuoted

readExpr :: String -> Element
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> StringElement $ "No match: " ++ show err
  Right val -> val

showElement :: Element -> String
showElement (AtomElement    s) = s
showElement (IntElement     i) = show i
showElement (FloatElement   f) = show f
showElement (StringElement  s) = "\"" ++ s ++ "\""
showElement (VectorElement  v) = "[" ++ unwords (map showElement v) ++ "]"
showElement (ListElement    v) = "(" ++ unwords (map showElement v) ++ ")"
showElement (KeywordElement k) = ':' : k
showElement (QuotedElement  e) = '\'' : showElement e
