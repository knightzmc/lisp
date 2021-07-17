module Main where

import System.Environment
import Lib
import Execute

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
