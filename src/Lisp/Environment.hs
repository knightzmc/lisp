module Lisp.Environment where

import Control.Monad.Except (ExceptT, liftIO, liftM, runExceptT, throwError)
import Data.Functor ((<&>))
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Lisp.AST (Element)
import Lisp.Errors (LispError (..), ThrowsError, extractValue, trapError)

type Env = IORef (Map String (IORef Element))

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

nullEnv :: IO Env
nullEnv = newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . Map.lookup var)

getVar :: Env -> String -> IOThrowsError Element
getVar envRef varName = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Unbound variable" varName)
    (liftIO . readIORef)
    (Map.lookup varName env)

setVar :: Env -> String -> Element -> IOThrowsError Element
setVar envRef varName value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting unbound variable" varName)
    (liftIO . flip writeIORef value)
    (Map.lookup varName env)
  return value

defineVar :: Env -> String -> Element -> IOThrowsError Element
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef (Map.insert var valueRef env)
      return value

bindVars :: Env -> [(String, Element)] -> IO Env
bindVars envRef bindings = do
  newBindings <- Map.fromList <$> mapM addBinding bindings
  env <- readIORef envRef
  let newMap = Map.union env newBindings
  writeIORef envRef newMap
  return envRef
  where
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
