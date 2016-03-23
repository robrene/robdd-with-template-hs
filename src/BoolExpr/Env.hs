module BoolExpr.Env
    ( Env
    , (!)
    , null
    , size
    , member
    , notMember
    , lookup
    , empty
    , singleton
    , mkEnv
    , set
    , delete
    , elems
    , vars
    , assocs
    , toList
    , fromList
    ) where

import Prelude hiding (null, lookup)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)

{-
`Env` describes an environment of (natural) numbered variables mapped to
boolean values. The interface is similar to that of `Data.IntMap`.
-}

newtype Env = Env (IntMap Bool) deriving (Show, Eq)

--
-- Operators:

-- Find the value at a key.
(!) :: Env -> Int -> Bool
(!) (Env imap) i = imap IntMap.! i

--
-- Query:

-- Is the environment empty?
null :: Env -> Bool
null (Env imap) = IntMap.null imap

-- Number of elements in the environment.
size :: Env -> Int
size (Env imap) = IntMap.size imap

-- Is a variable with a number a member of the environment?
member :: Int -> Env -> Bool
member i (Env imap) = IntMap.member i imap

-- Is a variable with a number not a member of the environment?
notMember :: Int -> Env -> Bool
notMember i (Env imap) = IntMap.notMember i imap

-- Lookup the value of a variable with a number in the environment.
lookup :: Int -> Env -> Maybe Bool
lookup i (Env imap) = IntMap.lookup i imap

--
-- Construction:

-- The empty environment.
empty :: Env
empty = Env IntMap.empty

-- An environment of one element.
singleton :: Int -> Bool -> Env
singleton i _ | i < 0 = error "Can only use natural numbers as variable numbers"
singleton i b = Env $ IntMap.singleton i b

-- An environment of ascending numbered variable mappings.
mkEnv :: [Bool] -> Env
mkEnv bs = mkEnv' 0 bs empty
  where mkEnv' _ []     res = res
        mkEnv' i (b:ys) res = mkEnv' (i+1) ys $ set i b res

--
-- Update:

-- Set the boolean value for a numbered variable in the environment.
set :: Int -> Bool -> Env -> Env
set i _ _ | i < 0 = error "Can only use natural numbers as variable numbers"
set i b (Env imap) = Env $ IntMap.insert i b imap

-- Delete a numbered variable from the environment.
delete :: Int -> Env -> Env
delete i (Env imap) = Env $ IntMap.delete i imap

--
-- Conversion:

-- Return all boolean values of the environment in ascending order of their
-- keys.
elems :: Env -> [Bool]
elems (Env imap) = IntMap.elems imap

-- Return all variable numbers of the environment in ascending order.
vars :: Env -> [Int]
vars (Env imap) = IntMap.keys imap

-- Return all (variable numbers,boolean value) pairs of the environment in
-- ascending order.
assocs :: Env -> [(Int, Bool)]
assocs (Env imap) = IntMap.assocs imap

-- Convert the environment to a list of (variable numbers,boolean value) pairs.
toList :: Env -> [(Int, Bool)]
toList (Env imap) = IntMap.toList imap

-- Create a map from a list of (variable numbers,boolean value) pairs.
fromList :: [(Int, Bool)] -> Env
fromList xs | any (\(i, _) -> i < 0) xs = error "Can only use natural numbers as variable numbers"
fromList xs = Env $ IntMap.fromList xs
