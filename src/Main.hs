{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
newtype MalIO a = MalIO {
    runMalIO :: ExceptT MalError (StateT Env IO) a
} deriving (MonadState Env, Monad, Functor, Applicative, MonadIO, MonadError MalError)

type ErrorStateM m a = ErrorM (StateT Env m) a

main = repl initEnv

repl :: Env -> IO ()
repl env = do
    (stepResult, newEnv) <- runStep env
    showResult stepResult
    repl newEnv
  where
    runStep = runStateT $ runExceptT (runMalIO step)
    step = do
        line <- liftIO $ prompt "user> "
        val <- readExpr line
        eval val
    showResult = either print (putStrLn . prettyPrint)

prompt :: String -> IO String
prompt p = flushStr p >> getLine
  where
    flushStr str = putStr str >> hFlush stdout

eval :: MalVal -> MalIO MalVal
eval (List [Symbol "quote", val]) = return val
eval (List [Symbol "if", pred, trueExpr, falseExpr]) = ifForm pred trueExpr falseExpr
eval (List [Symbol "if", pred, trueExpr]) = ifForm pred trueExpr Nil
eval (List (Symbol "do":exprs))   = doForm exprs
eval (List [Symbol "let*", List bindings, expr]) = get >>= letStar bindings expr
eval (List [Symbol "def!", Symbol var, val]) = define var val
eval val@(Symbol var) = get >>= \env ->
    case find var env of
        Nothing -> throwError $ UnresolvedSymbol var
        Just val -> return val
eval val@(List []) = return val
eval val@(List list) = do
    newList <- mapM eval list
    let applied = apply newList
    either throwError return applied
eval val = return val

{- SPECIAL FORMS -}

-- do
--
-- (do <expr> ...)
--
-- Evaluates each expression from left to right, returning
-- the result of the last expression.
doForm [] = return Nil
doForm exprs = do
    values <- mapM eval exprs
    return $ last values

-- if
--
-- (if <pred> <expr1> [<expr2>])
--
-- If <pred> evaluates to anything other than false or nil, the
-- value of <expr1> will be returned. Otherwise <expr2> will
-- be evaluated and returned.
--
-- If <expr2> is unspecified and <pred> is falsey, then the expression
-- evaluates to nil.
ifForm pred trueExpr falseExpr = do
    val <- eval pred
    case val of
        Nil -> eval falseExpr
        Bool False -> eval falseExpr
        _ -> eval trueExpr

-- def!
--
-- (def! <var> <expr>)
--
-- Evaluates <expr> and binds its value to <var> in the current environment.
define :: String -> MalVal -> MalIO MalVal
define var val = do
    newVal <- eval val
    env <- get
    put (insert var newVal env)
    return newVal

-- let*
--
-- (let* (<var> <expr> ...) <body>)
--
-- let* creates a new environment, binds each <expr> to the preceding
-- <var> in the bindings list, and then evaluates <body> in this new
-- environment.
letStar :: [MalVal]      -- ^The list of bindings
        -> MalVal        -- ^The expression to evaluate
        -> Env           -- ^The parent environment
        -> MalIO MalVal
letStar bindings expr env = do
    put $ withParent env
    evalBindings bindings
    evalAndRestore (eval expr)
  where
    -- After evaluating <body> we must be sure to restore the parent
    -- environment, including in the error case.
    evalAndRestore action = do
        val <- catchError action restoreEnv
        put env
        return val

    restoreEnv err = put env >> throwError err

    evalBindings (Symbol var:expr:rest) = do
        val <- eval expr
        env <- get
        put (insert var val env)
        evalBindings rest
    evalBindings [] = return ()
    evalBindings _  = throwError $ BadForm "invalid bindings list."

apply :: [MalVal] -> Either MalError MalVal
apply (Func (Fn f):args) = f args

initEnv :: Env
initEnv = foldl (\env (var, f) -> insert var (Func (Fn f)) env) empty builtins
    where builtins = [("+", binOp (+))
                     ,("-", binOp (-))
                     ,("*", binOp (*))
                     ,("/", binOp Prelude.div)
                     ,(">", compareOp (>))
                     ,("<", compareOp (<))
                     ,("=", equals)
                     ]

binOp :: (Integer -> Integer -> Integer) -> [MalVal] -> Either MalError MalVal
binOp op [Number a, Number b] = return . Number $ op a b
binOp _ _ = throwError BadArgs

compareOp :: (Integer -> Integer -> Bool) -> [MalVal] -> Either MalError MalVal
compareOp op [Number a, Number b] = return . Bool $ a `op` b
compareOp _ _ = throwError BadArgs

equals [a, b] = return . Bool $ a == b
equals _ = throwError BadArgs
