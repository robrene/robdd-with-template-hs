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
  , deleteA
  , deleteB
  , naturalKeys
  , reverseKeys
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

-- Lookup a value in the natural direction.
(!) :: BiDLUT a b -> a -> Maybe b
(!) (MkBiDLUT ab _) a = Prelude.lookup a ab

-- Lookup a value in the opposite direction.
(¡) :: BiDLUT a b -> b -> Maybe a
(¡) (MkBiDLUT _ ba) b = Prelude.lookup b ba

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

-- Delete a relation from the lookup table by its natural key.
deleteA :: a -> BiDLUT a b -> BiDLUT a b
deleteA a lut@(MkBiDLUT ab ba) =
  let b = lut ! a
   in if isNothing b then lut
      else MkBiDLUT (List.delete (a, fromJust b) ab) (List.delete (fromJust b, a) ba)

-- Delete a relation from the lookup table by its reverse key.
deleteB :: b -> BiDLUT a b -> BiDLUT a b
deleteB b lut@(MkBiDLUT ab ba) =
  let a = lut ¡ b
   in if isNothing a then lut
      else MkBiDLUT (List.delete (fromJust a, b) ab) (List.delete (b, fromJust a) ba)

--
-- Conversion:

-- Return all natural keys.
naturalKeys :: BiDLUT a b -> [a]
naturalKeys (MkBiDLUT ab _) = fst $ unzip ab

-- Return all reverse keys.
reverseKeys :: BiDLUT a b -> [b]
reverseKeys (MkBiDLUT ab _) = snd $ unzip ab
