{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Data.Functor.Identity

import Text.ParserCombinators.Parsec (Parser)

import Parse
import MalVal
import Environment

-- We want to have the state propagated regardless of if there is an error or not.
-- This means we want ExceptT e (StateT s m) a
--
-- This is isomorphic to m (Either e a, s), so we get the new state back regardless
-- of whether our operations have succeeded or not.
--
-- The other case would give us an m (Either e (a, s)), so our state would not be
-- persisted in the case of errors.
--
-- e = MalError
-- s = Env
-- m = IO
-- a = MalVal
newtype Mal a = Mal {
    runMal :: ExceptT MalError (StateT Env IO) a
} deriving (MonadState Env, Monad, Functor, Applicative, MonadIO, MonadError MalError)

type ErrorStateM m a = ErrorM (StateT Env m) a

main = repl initEnv

repl :: Env -> IO ()
repl env = do
    (stepResult, newEnv) <- runStep env
    showResult stepResult
    repl newEnv
  where
    runStep = runStateT $ runExceptT (runMal step)
    step = do
        line <- liftIO $ prompt "user> "
        val <- readExpr line
        env <- get
        eval env val
    showResult = either print (putStrLn . prettyPrint)

prompt :: String -> IO String
prompt p = flushStr p >> getLine
  where
    flushStr str = putStr str >> hFlush stdout

eval :: Env -> MalVal -> Mal MalVal
eval env (List [Symbol "quote", val]) = return val
eval env (List [Symbol "def!", Symbol var, val]) = do
    new_val <- eval env val
    new_env <- get
    put (insert var new_val new_env)
    return (Symbol "ok")
eval env val@(Symbol var) =
    case find var env of
        Nothing -> throwError $ UnresolvedSymbol var
        Just val -> return val
eval env val@(List []) = return val
eval env val@(List list) = mapM (eval env) list >>= apply
eval env val = return val

apply :: [MalVal] -> Mal MalVal
apply = return . List

initEnv :: Env
initEnv = foldl (\env (var, val) -> insert var val env) empty builtins
    where builtins = [("+", Func (Fn add))]

add :: [MalVal] -> ErrorM IO MalVal
add [Number a, Number b] = return . Number $ a + b
add _ = throwError BadArgs
