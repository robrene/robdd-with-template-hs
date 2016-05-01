{-# LANGUAGE GADTs #-}

module Util.BiDLUT
  ( BiDLUT
  , (!)
  , (¡)
  , null
  , size
  , elemA
  , elemB
  , empty
  , singleton
  , set
  , naturalKeys
  , reverseKeys
  , assocs
  ) where

import Prelude hiding (null)
import qualified Prelude (null)
import qualified Data.List as List (delete)
import Data.Maybe

{-
Bi-directional lookup table, allows fast lookups in both directions. Both sides
of the lookup table must be unique with respect to their side in order to
function as key.
-}

data BiDLUT a b where
  MkBiDLUT :: (Eq a, Eq b) => [(a, b)] -> [(b, a)] -> BiDLUT a b

--
-- Operators:

-- Find the value at a key in the natural direction.
(!) :: BiDLUT a b -> a -> b
(!) (MkBiDLUT ab _) a =
  let res = Prelude.lookup a ab
   in if isJust res then fromJust res
      else error $ "Given key is not an element of the lookup table"

-- Find the value at a key in the opposite direction.
(¡) :: BiDLUT a b -> b -> a
(¡) (MkBiDLUT _ ba) b =
  let res = Prelude.lookup b ba
   in if isJust res then fromJust res
      else error $ "Given key is not an element of the lookup table"

--
-- Query:

-- Is the lookup table empty?
null :: BiDLUT a b -> Bool
null (MkBiDLUT ab _) = Prelude.null ab

-- Number of entries in the lookup table.
size :: BiDLUT a b -> Int
size (MkBiDLUT ab _) = length ab

-- Does the lookup table contain a natural key?
elemA :: a -> BiDLUT a b -> Bool
elemA a (MkBiDLUT ab _) = any (\(a', _) -> a' == a) ab

-- Does the lookup table contain a reverse key?
elemB :: b -> BiDLUT a b -> Bool
elemB b (MkBiDLUT ab _) = any (\(_, b') -> b' == b) ab

--
-- Construction:

-- The empty lookup table.
empty :: (Eq a, Eq b) => BiDLUT a b
empty = MkBiDLUT [] []

-- A lookup table of one relation.
singleton :: (Eq a, Eq b) => a -> b -> BiDLUT a b
singleton a b = MkBiDLUT [(a, b)] [(b, a)]

--
-- Update:

-- Put a new relation in the lookup table. Existing relations that equal to any
-- of the keys will be updated. This operation can decrease the size of the
-- lookup table!
set :: a -> b -> BiDLUT a b -> BiDLUT a b
set a b (MkBiDLUT ab ba) =
  let ab' = List.delete (a, b) ab
      ba' = List.delete (b, a) ba
   in MkBiDLUT ((a, b):ab') ((b, a):ba')

-- TODO: implement safeSet that throws error on existing key/reverse key

--
-- Conversion:

-- Return all natural keys.
naturalKeys :: BiDLUT a b -> [a]
naturalKeys (MkBiDLUT ab _) = fst $ unzip ab

-- Return all reverse keys.
reverseKeys :: BiDLUT a b -> [b]
reverseKeys (MkBiDLUT ab _) = snd $ unzip ab

--
assocs :: BiDLUT a b -> [(a, b)]
assocs (MkBiDLUT ab _) = ab

--
-- Instances:

instance (Show a, Show b) => Show (BiDLUT a b) where
  show (MkBiDLUT ab _) = unlines $ map show ab
