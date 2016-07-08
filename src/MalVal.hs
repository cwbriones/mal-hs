module MalVal where

import Control.Monad.Except

import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)

newtype Fn = Fn ([MalVal] -> ErrorM IO MalVal)
data MalVal
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | List [MalVal]
  | Nil
  | Func Fn

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

showError :: MalError -> String
showError (Parser p) = show p
showError (UnresolvedSymbol var) = "Unable to resolve symbol \'" ++ var ++ "\' in this context."

instance Show MalError where
  show e = "Error: " ++ showError e

type ErrorM m = ExceptT MalError m
