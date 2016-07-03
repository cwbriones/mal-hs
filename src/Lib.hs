module Lib
    ( repl
    ) where

import Control.Monad
import System.IO

repl :: IO ()
repl = forever $ do
  line <- prompt
  let evald = eval line
  putStrLn evald

eval :: String -> String
eval = id

prompt :: IO String
prompt = do
  putStr "> "
  hFlush stdout
  getLine
