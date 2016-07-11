module Environment
( Env(..)
, empty
, extend
, find
, insert
) where

import Control.Monad
import qualified Data.Map as M

data Env a = Env { scope :: M.Map String a
                 , parent :: Maybe (Env a)
                 }
  deriving (Show)

{- Creates an empty environment with no parent. -}
empty :: Env a
empty = Env { scope = M.empty
            , parent = Nothing
            }

{- Creates an empty environment with the given parent. -}
extend :: Env a -> Env a
extend p = empty { parent = Just p }

{- Finds the value of var in env, if any. -}
find :: String -> Env a -> Maybe a
find var env =
  M.lookup var (scope env) `mplus` (parent env >>= find var)

{- Creates or updates the variable binding in the given Environment -}
insert :: String -> a -> Env a -> Env a
insert var val env = env { scope = M.insert var val (scope env) }
