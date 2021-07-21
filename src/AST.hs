-- |

module AST where

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

showElement :: Element -> String
showElement (AtomElement    s) = s
showElement (IntElement     i) = show i
showElement (FloatElement   f) = show f
showElement (StringElement  s) = "\"" ++ s ++ "\""
showElement (VectorElement  v) = "[" ++ unwords (map showElement v) ++ "]"
showElement (ListElement    v) = "(" ++ unwords (map showElement v) ++ ")"
showElement (KeywordElement k) = ':' : k
showElement (QuotedElement  e) = '\'' : showElement e
