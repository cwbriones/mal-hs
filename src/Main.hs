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

main = repl initEnv

repl :: MalEnv -> IO ()
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
    showResult = either print print

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
eval (List [Symbol "fn*", List bindings, expr]) = makeLambda bindings expr
eval val@(Symbol var) = get >>= \env ->
    case find var env of
        Nothing -> throwError $ UnresolvedSymbol var
        Just val -> return val
eval val@(List []) = return val
eval val@(List list) = mapM eval list >>= apply
eval val = return val

apply :: [MalVal] -> MalIO MalVal
apply (Func (Fn f):args) = f args
apply (Lambda env vars expr:args) = do
    checkArgs (length vars) (length args)
    withinEnv boundEnv (eval expr)
  where
    boundEnv = foldl bind env (zip vars args)
    bind e (var, val) = insert var val e
    checkArgs a b = unless (a == b) $ throwError $ BadForm "Wrong number of arguments"
apply (val:_) = throwError $ CannotApply val

--
-- Evaluates an action within the context of a temporary environment.
--
withinEnv :: Env MalVal   -- ^The environment to perform our action within
          -> MalIO MalVal -- ^The action to perform
          -> MalIO MalVal
withinEnv newEnv action = do
    env <- get
    put newEnv
    val <- catchError action (restoreEnv env)
    put env
    return val
  where
    restoreEnv env err = put env >> throwError err

{- SPECIAL FORMS -}

-- fn*
--
-- (fn* (<var> ...) <expr>)
--
-- Creates a lambda with the given parameter list and body.
--
-- This isn't entirely correct for this reason:
-- Pretend we want to make a recursive function like so:
--
-- (defn! fac (n) (if (= n 0) 1 (* n (fac (- n 1)))))
--
-- simplify to...
-- (def fac <recursive-expr>)
--
-- Let's trace execution:
-- globals = []
-- (defn! fac (n) <rescursive-expr>)
--   Create a lambda with parent=globals params=(n), body=<expr>
-- globals' = (fac #<lambda>)
--
-- See now, we have defined fac but it's parent environment still
-- refers to the *previous* parent env which doesn't have fac inside of it.
--
-- Because of this, when we try to recursively call fac, the inner env in
-- the lambda will have the parent globals which is empty and the call will fail.
--
-- In this way we somehow need to bind the environment AFTER the lambda is created,
-- and yet, the lambda needs to hold a reference to the outer env...
makeLambda :: [MalVal] -> MalVal -> MalIO MalVal
makeLambda bindings expr = do
    env <- get
    vars <- extractBindings bindings
    return $ Lambda (withParent env) vars expr
  where
    extractBindings = mapM extract
    extract (Symbol s) = return s
    extract _ = throwError $ BadForm "lambda parameters must be symbols."

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
        -> MalEnv        -- ^The parent environment
        -> MalIO MalVal
letStar bindings expr env =
    withinEnv childEnv bindAndEval
  where
    childEnv = withParent env
    bindAndEval = evalBindings bindings >> eval expr
    evalBindings (Symbol var:expr:rest) = do
        val <- eval expr
        env <- get
        put (insert var val env)
        evalBindings rest
    evalBindings [] = return ()
    evalBindings _  = throwError $ BadForm "invalid bindings list."

{- Built-in Functions -}

initEnv :: MalEnv
initEnv = foldl (\env (var, f) -> insert var (Func (Fn f)) env) empty builtins
    where builtins = [("+", binOp (+))
                     ,("-", binOp (-))
                     ,("*", binOp (*))
                     ,("/", binOp Prelude.div)
                     ,(">", compareOp (>))
                     ,("<", compareOp (<))
                     ,(">=", compareOp (>=))
                     ,("<=", compareOp (<=))
                     ,("=", equals)
                     ,("list", list)
                     ,("list?", isList)
                     ,("count", count)
                     ,("empty?", isEmpty)
                     ]

binOp :: (Int -> Int -> Int) -> [MalVal] -> MalIO MalVal
binOp op [Number a, Number b] = return . Number $ op a b
binOp _ _ = throwError BadArgs

compareOp :: (Int -> Int -> Bool) -> [MalVal] -> MalIO MalVal
compareOp op [Number a, Number b] = return . Bool $ a `op` b
compareOp _ _ = throwError BadArgs

equals [a, b] = return . Bool $ a == b
equals _ = throwError BadArgs

list = return . List

isList [List _] = return (Bool True)
isList [_] = return (Bool False)
isList _ = throwError BadArgs

count [List list] = return $ Number (length list)
count [Nil] = return $ Number 0
count _ = throwError BadArgs

isEmpty [List list] = return $ Bool . null $ list
isEmpty _ = throwError BadArgs
