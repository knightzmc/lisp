module Lisp.Repl where

import Lisp.Errors (trapError)
import Lisp.Execute
import Lisp.Parse (readExpr)
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condition prompt action = do
  result <- prompt
  if condition result
    then return ()
    else action result >> until_ condition prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
