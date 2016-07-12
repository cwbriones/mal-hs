{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Data.Functor.Identity

import Text.ParserCombinators.Parsec (Parser)
import qualified System.Console.Haskeline as HL

import Parse
import MalVal
import Environment

main = HL.runInputT HL.defaultSettings $ repl initEnv

repl :: MalEnv -> HL.InputT IO ()
repl env = do
    minput <- HL.getInputLine "user> "
    case minput of
        Nothing -> return ()
        Just line -> do
            (result, newEnv) <- liftIO $ runStep line env
            showResult result
            repl newEnv
  where
    runStep input = runStateT $ runExceptT (runMalIO (step input))
    step input = do
        val <- readExpr input
        eval val
    showResult = either hlprint hlprint

    hlprint :: (Show a) => a -> HL.InputT IO ()
    hlprint = HL.outputStrLn . show

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
apply (Lambda vars expr:args) = do
    env <- get
    checkArgs (length vars) (length args)
    withinEnv (boundEnv env) (eval expr)
  where
    boundEnv e = foldl bind (extend e) (zip vars args)
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
makeLambda :: [MalVal] -> MalVal -> MalIO MalVal
makeLambda bindings expr = do
    vars <- extractBindings bindings
    return $ Lambda vars expr
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
    childEnv = extend env
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
