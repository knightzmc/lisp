module Main where

import System.Environment
import Lisp.Parse
import Lisp.Execute
import Lisp.Errors

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
