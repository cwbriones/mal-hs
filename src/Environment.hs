module Environment
( Env(..)
, empty
, withParent
, find
, insert
) where

import Control.Monad
import qualified Data.Map as M

import MalVal

data Env = Env { scope :: M.Map String MalVal
               , parent :: Maybe Env
               }

{- Creates an empty environment with no parent. -}
empty :: Env
empty = Env { scope = M.empty
            , parent = Nothing
            }

{- Creates an empty environment with the given parent. -}
withParent :: Env -> Env
withParent p = empty { parent = Just p }

{- Finds the value of var in env, if any. -}
find :: String -> Env -> Maybe MalVal
find var env =
  M.lookup var (scope env) `mplus` (parent env >>= find var)

{- Creates or updates the variable binding in the given Environment -}
insert :: String -> MalVal -> Env -> Env
insert var val env = env { scope = M.insert var val (scope env) }
