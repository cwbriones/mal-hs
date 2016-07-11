module IOEnv
( IOEnv(..)
, empty
, extend
, find
, insert
) where

import Control.Monad
import Data.IORef
import qualified Data.Map as M

import MalVal

data IOEnv = IOEnv { scope :: IORef InnerEnv
                   , parentScope :: Maybe IOEnv
                   }

type InnerEnv = M.Map String MalVal

empty :: IO IOEnv
empty = do
    newScope <- newIORef M.empty
    return IOEnv{scope = newScope, parentScope = Nothing}

extend :: IOEnv -> IO IOEnv
extend topEnv = do
    newScope <- newIORef M.empty
    return IOEnv{scope = newScope, parentScope = Just topEnv}

insert :: String -> MalVal -> IOEnv -> IO MalVal
insert var val envRef = modifyIORef (scope envRef) (M.insert var val) >> return val

find :: String -> IOEnv -> IO (Maybe MalVal)
find var envRef = do env <- readIORef (scope envRef)
                     findInner var env `mplus` findParent var
  where
    findParent var =
        case parentScope envRef of
          Nothing -> return Nothing
          Just p  -> find var p

    findInner var env = return $ M.lookup var env
