module Environment
( Env(..)
, empty
, extend
, find
, insert
) where

import Control.Monad
import Data.IORef
import qualified Data.Map as M

data Env a = Env { scope :: IORef (InnerEnv a)
                 , outer :: Maybe (Env a)
                 }

type InnerEnv a = M.Map String a

empty :: IO (Env a)
empty = do
    newScope <- newIORef M.empty
    return Env{scope = newScope, outer = Nothing}

extend :: Env a -> IO (Env a)
extend topEnv = do
    newScope <- newIORef M.empty
    return Env{scope = newScope, outer = Just topEnv}

insert :: String -> a -> Env a -> IO ()
insert var val envRef = modifyIORef (scope envRef) (M.insert var val)

find :: String -> Env a -> IO (Maybe a)
find var envRef = do env <- readIORef (scope envRef)
                     findInner var env `ioplus` findParent var
  where
    ioplus = liftM2 mplus

    findParent var =
        case outer envRef of
            Nothing -> return Nothing
            Just p  -> find var p

    findInner var env = return $ M.lookup var env
