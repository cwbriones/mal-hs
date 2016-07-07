module MalVal where

import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)
import Control.Monad.Except

data MalVal
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | List [MalVal]
  | Nil
  deriving (Read, Eq, Show)

prettyPrint :: MalVal -> String
prettyPrint (Symbol s) = s
prettyPrint (Number n) = show n
prettyPrint (String s) = show s
prettyPrint (Bool b) = if b then "true" else "false"
prettyPrint Nil = "nil"
prettyPrint (List vals) = "(" ++ printAll vals ++ ")"
  where printAll = unwords . map prettyPrint

data MalError
  = Parser ParseError

showError :: MalError -> String
showError (Parser p) = show p

instance Show MalError where
  show = showError

type ErrorM m = ExceptT MalError m
