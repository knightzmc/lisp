module Lisp.AST where

data Element
  = Atom String
  | Int Integer
  | Float Double
  | String String
  | Bool Bool
  | Vector [Element]
  | List [Element]
  | Quote Element
  deriving (Eq)

instance Show Element where
  show = showElement

showElement :: Element -> String
showElement (Atom    s) = s
showElement (Int     i) = show i
showElement (Float   f) = show f
showElement (Bool b) = if b then "true" else "false"
showElement (String  s) = "\"" ++ s ++ "\""
showElement (Vector  v) = "[" ++ unwords (map showElement v) ++ "]"
showElement (List    v) = "(" ++ unwords (map showElement v) ++ ")"
--showElement (Keyword k) = ':' : k
showElement (Quote  e) = '\'' : showElement e
