{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Exception (try)
import System.IO
import Data.Functor.Identity
import Data.IORef

import Text.ParserCombinators.Parsec (Parser)
import qualified System.Console.Haskeline as HL

import Parse
import MalVal
import Environment

main = HL.runInputT HL.defaultSettings hlrepl
  where
    hlrepl = liftIO initializeEnv >>= repl

repl :: MalEnv -> HL.InputT IO ()
repl env = do
    minput <- HL.getInputLine "user> "
    case minput of
        Nothing -> return ()
        Just line -> do
            (result, newEnv) <- liftIO $ run line env
            showResult result
            repl newEnv
  where
    run input = runStateT $ runExceptT $ runMalIO $ step input
    step input = do
        val <- readExpr input
        eval' val

    showResult = either hlprint hlprint

    hlprint :: (Show a) => a -> HL.InputT IO ()
    hlprint = HL.outputStrLn . show

evalString :: String -> MalIO MalVal
evalString s =
    readManyExpr s >>= doForm >>= eval'

{-| Fully evaluate a mal expression. -}
eval' :: MalVal -> MalIO MalVal
eval' val = do
    evaluated <- eval val
    unthunk evaluated
  where
    unthunk (Thunk env expr) = do
        evaluated <- evalWithin env expr
        unthunk evaluated
    unthunk val = return val

-- If there's an effectful form in tail-position then it won't actually
-- affect the top-level environment without this check. This isn't a fault
-- of the do-form but is the only case where it can actually happen.
--
-- This is because the environment is replaced when it is in a thunk and
-- the effects never propagate past evaluation.
--
-- example:
-- (do (def! a 1))
-- => 1
-- a
-- => Error: Unable to resolve symbol 'a' in this context.
    evalWithin env expr =
        if isRoot env
            then eval expr
            else withinEnv env (eval expr)

{-| Evaluate a mal expression, possibly returning a thunk. -}
eval :: MalVal -> MalIO MalVal
eval val@(Symbol var) = get >>= \env ->
    case find var env of
        Nothing -> throwError $ UnresolvedSymbol var
        Just val -> return val
eval val@(List []) = return val
eval (List list) = evalList list
eval val = return val

evalList :: [MalVal] -> MalIO MalVal
evalList [Symbol "quote", val] = quote val
evalList [Symbol "quasiquote", val] = quasiquote val
evalList [Symbol "unquote", val] = throwError UnquoteError
evalList [Symbol "splice-unquote", val] = throwError UnquoteError
evalList [Symbol "if", pred, trueExpr, falseExpr] = ifForm pred trueExpr falseExpr
evalList [Symbol "if", pred, trueExpr] = ifForm pred trueExpr Nil
evalList (Symbol "do":exprs)   = doForm exprs
evalList [Symbol "let*", List bindings, expr] = get >>= letStar bindings expr
evalList [Symbol "def!", Symbol var, val] = eval' val >>= define var
evalList [Symbol "defn!", Symbol var, List params, expr] = makeLambda params expr >>= define var
evalList [Symbol "fn*", List params, expr] = makeLambda params expr
evalList list = mapM eval' list >>= apply

quote (List (Symbol "vector":vals)) = vector vals
quote (List (Symbol "hash-map":vals)) = makeHashmap vals
quote val = return val

quasiquote (List [Symbol "unquote", val]) = eval' val
quasiquote (List list) = quasiquoteList list []
  where
    quasiquoteList [] acc = return $ List $ reverse acc
    quasiquoteList (List [Symbol "splice-unquote", val]:exprs) acc = do
        evaluated <- eval' val
        case evaluated of
          List list -> quasiquoteList exprs (reverse list ++ acc)
          _ -> throwError SpliceUnquoteError
    quasiquoteList (expr:exprs) acc = do
        quoted <- quasiquote expr
        quasiquoteList exprs $! (quoted:acc)
quasiquote val = return val

apply :: [MalVal] -> MalIO MalVal
apply [sym@(Symbol k), term] = hashmapGet [term, sym]
apply [sym@(Symbol k), term, notFound] = hashmapGet [term, sym, notFound]
apply (Func Fn{f = f}:args) = f args
apply (Lambda outer vars expr:args) = do
    let env = boundEnv $ extend outer
    checkArgs (length vars) (length args)
    return $ Thunk env expr
  where
    boundEnv e = foldl bind e (zip vars args)
    bind e (var, val) = insertRec var val e
    checkArgs a b = unless (a == b) $ throwError $ BadForm "Wrong number of arguments"
apply (val:_) = throwError $ CannotApply val

swap :: [MalVal] -> MalIO MalVal
swap (atom@(Atom ref):f:varargs) = do
    atomVal <- liftIO $ readIORef ref
    applied <- apply (f:atomVal:varargs)
    evaluated <- eval' applied
    liftIO $ writeIORef ref evaluated
    return evaluated
swap _             = throwError BadArgs

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
    env <- get
    vars <- extractBindings bindings
    return $ Lambda env vars expr
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
--
doForm [] = return Nil
doForm [expr] = do
    env <- get
    return $ Thunk env expr
doForm (expr:exprs) = eval' expr >> doForm exprs

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
    val <- eval' pred
    env <- get
    case val of
        Nil -> thunk env falseExpr
        Bool False -> thunk env falseExpr
        _ -> thunk env trueExpr
  where
    thunk env expr = return $ Thunk env expr

-- def!
--
-- (def! <var> <expr>)
--
-- Evaluates <expr> and binds its value to <var> in the current environment.
define :: String -> MalVal -> MalIO MalVal
define var val = do
    modify (insertRec var val)
    return val

-- Insert a value into an environment, properly defining the environment recursively
-- when inserting a lambda.
insertRec :: String -> MalVal -> Env MalVal -> Env MalVal
insertRec var val@(Lambda _ args expr) env =
    let new_env = insert var (Lambda new_env args expr) env
        in
    new_env
insertRec var val env = insert var val env

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
    bindAndEval = evalBindings bindings >> eval' expr

    evalBindings (Symbol var:expr:rest) = do
        val <- eval expr
        modify (insertRec var val)
        evalBindings rest
    evalBindings [] = return ()
    evalBindings _  = throwError $ BadForm "invalid bindings list."

{- Built-in Functions -}

-- | Runs a single step of the interpreter with the contents of the standard library
-- defined in mal itself.
initializeEnv :: IO MalEnv -- ^Action loading the standard environment.
initializeEnv = run builtinsOnly
  where
    run = execStateT $ runExceptT (runMalIO $ initStdLib `catchError` printFailure)

    io = liftIO
    printFailure  _ = io $ print "error while loading standard library."

    initStdLib :: MalIO ()
    initStdLib = do
        io $ print "Loading standard library..."
        evalString standardLibrary
        io $ print "done."

    builtinsOnly = fst $ foldl defineBuiltin (empty, 0) builtins

    defineBuiltin (env, fid) (var, f) = (new_env, fid + 1)
        where new_env = insert var(Func Fn{f=f, fid=0}) env

    builtins = [("+", binOp (+))
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
               ,("concat", malconcat)
               ,("cons", cons)
               ,("pr-str", prStr)
               ,("prn", prn)
               ,("read-string", readString)
               ,("slurp", slurp)
               ,("eval", libeval)
               ,("str", str)
               ,("load-file", loadFile)
               ,("swap!", swap)
               ,("atom?", atomPred)
               ,("atom", atom)
               ,("deref", deref)
               ,("reset!", reset)
               ,("hash-map", makeHashmap)
               ,("get", hashmapGet)
               ,("assoc", hashmapPut)
               ,("dissoc", hashmapRemove)
               ,("contains?", hashmapContains)
               ,("keys", hashmapKeys)
               ,("vals", hashmapValues)
               ,("map?", mapPred)
               ,("vector", vector)
               ,("vector?", vectorPred)
               ,("conj", conj)
               ]

standardLibrary =
    "(def! not (fn* (x) (if x false true)))"

lprint = liftIO . print

binOp :: (Integer -> Integer -> Integer) -> [MalVal] -> MalIO MalVal
binOp op [Number a, Number b] = return . Number $ op a b
binOp _ args = lprint ("binop: " ++ show args) >> throwError BadArgs

compareOp :: (Integer -> Integer -> Bool) -> [MalVal] -> MalIO MalVal
compareOp op [Number a, Number b] = return . Bool $ a `op` b
compareOp _ _ = throwError BadArgs

equals [a, b] = return . Bool $ a == b
equals _ = throwError BadArgs

list :: [MalVal] -> MalIO MalVal
list = return . List

isList [List _] = return (Bool True)
isList [_] = return (Bool False)
isList _ = throwError BadArgs

count [List list] = return $ Number . toInteger $ length list
count [Nil] = return $ Number 0
count _ = throwError BadArgs

isEmpty [List list] = return $ Bool . null $ list
isEmpty _ = throwError BadArgs

cons [val, List list] = return $ List (val:list)
cons _ = throwError BadArgs

malconcat lists = go lists []
  where
    go [] acc = return . List . concat . reverse $ acc
    go (List list:lists) acc = go lists (list:acc)
    go _ _ = throwError BadArgs

str strings = (String . concat) <$> unbox strings []
  where
    unbox [] acc = return $ reverse acc
    unbox (String s:strings) acc = unbox strings (s:acc)
    unbox _ _ = throwError BadArgs

libeval [val] = eval' val
libeval _ = throwError BadArgs

prStr :: [MalVal] -> MalIO MalVal
prStr [val] = return . String . show $ val
prStr _     = throwError BadArgs

prn :: [MalVal] -> MalIO MalVal
prn [val] = liftIO $ print val >> return Nil
prn _     = throwError BadArgs

readString :: [MalVal] -> MalIO MalVal
readString [String expr] = readExpr expr
readString _ = throwError BadArgs

slurp :: [MalVal] -> MalIO MalVal
slurp [String path] = liftIOErr (readFile path) String
slurp _ = throwError BadArgs

{-| Loads a file and evaluates it within the current environment.
 -
 - This should be defined in Mal itself but because the environment
 - is built immutably we must make it a built-in. Otherwise the definitions
 - loaded would not be injected into the global environment. -}
loadFile :: [MalVal] -> MalIO MalVal
loadFile [String path] = do
    str <- liftIOErr (readFile path) id
    evalString str
loadFile _ = throwError BadArgs

liftIOErr :: IO a -> (a -> b) -> MalIO b
liftIOErr io f = do
    result <- liftIO $ try io
    either throwIOErr (return . f) result
  where
    throwIOErr err = throwError $ IO err
