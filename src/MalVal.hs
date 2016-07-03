module MalVal where

import Text.Parsec (try)
import Text.ParserCombinators.Parsec hiding (try, spaces)
import Control.Monad.Except

data MalVal
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | Pair MalVal MalVal
  | EmptyList
  | Nil
  deriving (Read, Eq, Show)

data MalError
  = Parser ParseError

prettyPrint :: MalVal -> String
prettyPrint (Symbol s) = s
prettyPrint (Number n) = show n
prettyPrint (String s) = s
prettyPrint (Bool b) = if b then "true" else "false"
prettyPrint EmptyList = "()"
prettyPrint Nil = "nil"
prettyPrint (Pair car cdr) = "(" ++ printPair car cdr ++ ")"

printPair car cdr@(Pair cdar cddr) = prettyPrint car ++ " " ++ printPair cdar cddr
printPair car EmptyList = prettyPrint car
printPair car cdr = prettyPrint car ++ " . " ++ prettyPrint cdr

showError :: MalError -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show MalError where show = showError

type MalResult = Either MalError MalVal

printResult :: MalResult -> IO ()
printResult (Left err)  = print err
printResult (Right val) = putStrLn $ prettyPrint val
