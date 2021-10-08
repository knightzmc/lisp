module Lisp.Repl where

import Lisp.Errors (trapError)
import Lisp.Execute
import Lisp.Parse (readExpr)
import System.IO
import Lisp.Environment (runIOThrows, liftThrows, Env, nullEnv)
import Control.Monad.Cont (liftM)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condition prompt action = do
  result <- prompt
  if condition result
    then return ()
    else action result >> until_ condition prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
