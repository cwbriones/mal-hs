{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MalVal where

import Control.Monad.Except
import Control.Monad.State

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

newtype Fn = Fn ([MalVal] -> MalIO MalVal)
data MalVal
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | List [MalVal]
  | Nil
  | Func Fn
  | Lambda MalEnv [String] MalVal

instance Eq MalVal where
    (==) (Symbol a) (Symbol b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (String a) (String b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) (List a) (List b) = a == b
    (==) Nil Nil = True
    (==) _ _ = False

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

instance Show MalVal where
    show = prettyPrint

data MalError
  = Parser ParseError
  | UnresolvedSymbol String
  | CannotApply MalVal
  | BadArgs
  | BadForm String

showError :: MalError -> String
showError (Parser p) = show p
showError (UnresolvedSymbol var) = "Unable to resolve symbol \'" ++ var ++ "\' in this context."
showError (CannotApply val) = "Cannot apply " ++ show val
showError BadArgs = "Bad arguments for function."
showError (BadForm msg) = "Invalid special form: " ++ msg

instance Show MalError where
  show e = "Error: " ++ showError e

type ErrorM m = ExceptT MalError m
