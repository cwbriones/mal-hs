module Main where

import Parse
import Text.ParserCombinators.Parsec (Parser)
import MalVal

import Control.Monad
import Control.Monad.Except
import System.IO

main :: IO ()
main = forever $ do
    r <- runExceptT repl
    showResult r
  where
    showResult (Left err)  = print err
    showResult (Right val) = putStrLn $ prettyPrint val

repl :: ErrorM IO MalVal
repl = do
  line <- liftIO $ prompt "user> "
  val <- readExpr line
  eval val

eval :: MalVal -> ErrorM IO MalVal
eval (List [Symbol "quote", val]) = return val
eval val = return val

prompt :: String -> IO String
prompt p = flushStr p >> getLine
  where
    flushStr str = putStr str >> hFlush stdout

