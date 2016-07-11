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

data IOEnv a = IOEnv { scope :: IORef (InnerEnv a)
                     , parentScope :: Maybe (IOEnv a)
                     }

type InnerEnv a = M.Map String a

empty :: IO (IOEnv a)
empty = do
    newScope <- newIORef M.empty
    return IOEnv{scope = newScope, parentScope = Nothing}

extend :: IOEnv a -> IO (IOEnv a)
extend topEnv = do
    newScope <- newIORef M.empty
    return IOEnv{scope = newScope, parentScope = Just topEnv}

insert :: String -> a -> IOEnv a -> IO a
insert var val envRef = modifyIORef (scope envRef) (M.insert var val) >> return val

find :: String -> IOEnv a -> IO (Maybe a)
find var envRef = do env <- readIORef (scope envRef)
                     findInner var env `ioplus` findParent var
  where
    ioplus = liftM2 mplus

    findParent var =
        case parentScope envRef of
          Nothing -> return Nothing
          Just p  -> find var p

    findInner var env = return $ M.lookup var env
