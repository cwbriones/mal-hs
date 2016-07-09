module MalVal where

import Control.Monad.Except

import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)

newtype Fn = Fn ([MalVal] -> Either MalError MalVal)
data MalVal
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | List [MalVal]
  | Nil
  | Func Fn

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
prettyPrint (List vals) = "(" ++ printAll vals ++ ")"
  where printAll = unwords . map prettyPrint

data MalError
  = Parser ParseError
  | UnresolvedSymbol String
  | CannotApply MalVal
  | BadArgs
  | BadForm String

showError :: MalError -> String
showError (Parser p) = show p
showError (UnresolvedSymbol var) = "Unable to resolve symbol \'" ++ var ++ "\' in this context."
showError BadArgs = "Bad arguments for function."
showError (BadForm msg) = "Invalid special form: " ++ msg

instance Show MalError where
  show e = "Error: " ++ showError e

type ErrorM m = ExceptT MalError m
