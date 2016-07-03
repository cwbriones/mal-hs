module Main where

import Parse
import MalVal

import Control.Monad
import System.IO

main :: IO ()
main = repl

repl :: IO ()
repl = forever $ do
  line <- prompt "mal> "
  printResult $ evalStr line

evalStr :: String -> MalResult
evalStr = readExpr

eval :: MalVal -> MalResult
eval = return

prompt :: String -> IO String
prompt p = flushStr p >> getLine
  where
    flushStr str = putStr str >> hFlush stdout

