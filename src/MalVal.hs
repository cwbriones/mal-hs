{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module MalVal where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isJust)

import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Vector as V
import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)

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
type MalEnv = Env MalVal

newtype MalIO a = MalIO {
    runMalIO :: ExceptT MalError (StateT MalEnv IO) a
} deriving (MonadState MalEnv, Monad, Functor, Applicative, MonadIO, MonadError MalError)

data Fn = Fn {fid :: Int, f :: [MalVal] -> MalIO MalVal}

instance Eq Fn where
    (==) Fn{fid = a} Fn{fid = b} = a == b

data MalVal
  = Symbol !String
  | Number !Integer
  | String !String
  | Bool !Bool
  | List ![MalVal]
  | Vector (V.Vector MalVal)
  | Map (HM.HashMap MalVal MalVal)
  | Nil
  | Func Fn
  | Lambda MalEnv ![String] !MalVal
  | Thunk MalEnv MalVal
  | Atom (IORef MalVal)
  deriving (Eq)

instance Hashable MalVal where
    -- String Constructors
    hashWithSalt s (Symbol x) = s `hashWithSalt` (0::Int) `hashWithSalt` x
    hashWithSalt s (String x) = s `hashWithSalt` (1::Int) `hashWithSalt` x
    -- Number Constructors
    hashWithSalt s (Number x) = s `hashWithSalt` (0::Int) `hashWithSalt` x
    hashWithSalt s (Func Fn{fid = fid}) = s `hashWithSalt` (1::Int) `hashWithSalt` fid
    hashWithSalt s Nil        = s `hashWithSalt` (2::Int) `hashWithSalt` False
    hashWithSalt s (Bool x)   = s `hashWithSalt` (3::Int) `hashWithSalt` x
    -- Misc
    hashWithSalt s (List x)   = s `hashWithSalt` (0::Int) `hashWithSalt` x
    hashWithSalt s (Vector x)   = V.foldl' hashWithSalt (s `hashWithSalt` (1::Int)) x
    hashWithSalt s (Lambda _ args expr) = s `hashWithSalt` args `hashWithSalt` expr
    -- We should never have to hash a Thunk.
    hashWithSalt _ (Thunk _ _) = error "Attempted to hash a thunk"
    -- Atoms not supported at the moment.
    hashWithSalt _ (Atom _)    = undefined

prettyPrint :: MalVal -> String
prettyPrint (Symbol s) = s
prettyPrint (Number n) = show n
prettyPrint (String s) = show s
prettyPrint (Bool b) = if b then "true" else "false"
prettyPrint Nil = "nil"
prettyPrint (Func _) = "#<builtin-function>"
prettyPrint Lambda{} = "#<lambda>"
prettyPrint (List vals) = "(" ++ printAll vals ++ ")"
  where printAll = unwords . map prettyPrint
prettyPrint (Vector vals) = "[" ++ printAll (V.toList vals) ++ "]"
  where printAll = unwords . map prettyPrint
prettyPrint (Map map) = "{" ++ prettyPrintInner map ++ "}"
  where
    prettyPrintInner map = unwords . reverse $ HM.foldrWithKey showKV [] map
    showKV k v s = show v:show k:s
prettyPrint Atom{} = "#<atom>"

instance Show MalVal where
    show = prettyPrint

data MalError
  = Parser ParseError
  | UnresolvedSymbol String
  | CannotApply MalVal
  | BadArgs
  | BadForm String
  | IO IOError
  | UnquoteError
  | SpliceUnquoteError

showError :: MalError -> String
showError (Parser p) = show p
showError (UnresolvedSymbol var) = "Unable to resolve symbol \'" ++ var ++ "\' in this context."
showError (CannotApply val) = "Cannot apply " ++ show val
showError BadArgs = "Bad arguments for function."
showError (BadForm msg) = "Invalid special form: " ++ msg
showError (IO err) = show err
showError UnquoteError = "Unquote called from outside quasiquote."
showError SpliceUnquoteError = "Expressions within splice-unquote must evaluate to a list."

instance Show MalError where
  show e = "Error: " ++ showError e

type ErrorM m = ExceptT MalError m

atom :: [MalVal] -> MalIO MalVal
atom [val] = do
    ref <- liftIO $ newIORef val
    let atom = Atom ref
    return atom
atom _     = throwError BadArgs

atomPred :: [MalVal] -> MalIO MalVal
atomPred [Atom _] = return $ Bool True
atomPred [_]      = return $ Bool False
atomPred _        = throwError BadArgs

deref :: [MalVal] -> MalIO MalVal
deref [Atom ref] = liftIO $ readIORef ref
deref _          = throwError BadArgs

reset :: [MalVal] -> MalIO MalVal
reset [atom@(Atom ref), val] = do
    liftIO $ atomicWriteIORef ref val
    return atom
reset _             = throwError BadArgs

makeHashmap :: [MalVal] -> MalIO MalVal
makeHashmap elements =
    if even $ length elements
        then return $ malmap $ pair elements
        else throwError BadArgs
  where
    pair [] = []
    pair (x:y:zs) = (x, y) : pair zs

hashmapGet :: [MalVal] -> MalIO MalVal
hashmapGet [map, key] = hashmapGet [map, key, Nil]
hashmapGet [Map map, key, notFound] =
    case HM.lookup key map of
        Just val -> return val
        Nothing  -> return notFound
hashmapGet _ = throwError BadArgs

hashmapPut :: [MalVal] -> MalIO MalVal
hashmapPut [Map map, key, val] = return . Map $ HM.insert key val map
hashmapPut _ = throwError BadArgs

hashmapRemove :: [MalVal] -> MalIO MalVal
hashmapRemove (Map map:keys) = return . Map $ removed map keys
  where
    removed = foldl' (flip HM.delete)
hashmapRemove _ = throwError BadArgs

hashmapContains :: [MalVal] -> MalIO MalVal
hashmapContains [Map map, key] = return . Bool $ isJust $ HM.lookup key map
hashmapContains _ = throwError BadArgs

hashmapKeys :: [MalVal] -> MalIO MalVal
hashmapKeys [Map map] = return . List $ HM.keys map
hashmapKeys _ = throwError BadArgs

hashmapValues :: [MalVal] -> MalIO MalVal
hashmapValues [Map map] = return . List $ HM.elems map
hashmapValues _ = throwError BadArgs

mapPred :: [MalVal] -> MalIO MalVal
mapPred [Map _] = return $ Bool True
mapPred [_] = return $ Bool False
mapPred _ = throwError BadArgs

malmap = Map . HM.fromList

vector vals = return . Vector $ V.fromList vals
