{-# LANGUAGE PatternSynonyms #-}

{-
Based on "An Introduction to Binary Decision Diagrams" by Henrik Reif Andersen.
-}

module BoolExpr.ROBDD
  ( NodeId
  , Ref (..)
  , NodeAttr
  , RefMap
  , ROBDD
  , build
  , eval
  , varPaths_
  ) where

import Control.Monad.State
import Data.Maybe

import BoolExpr.BoolExpr (BoolExpr, maximumVar)
import qualified BoolExpr.BoolExpr as BE (eval)
import BoolExpr.Env (Env, VarId)
import qualified BoolExpr.Env as Env (empty, set, (!))
import Util.BiDLUT as LUT

--
-- Data structure:

type NodeId = Int
data Ref = Ptr NodeId | Val Bool deriving (Eq, Show)
type NodeAttr = (VarId, Ref, Ref)
type RefMap = BiDLUT NodeId NodeAttr
type ROBDD = (Ref, RefMap)

--
-- Construction:

-- Make a new node in the ROBDD if necessary.
--
-- First, perform the "non-redundant tests".
-- Second, check for uniqueness by searching in the existing lookup table.
-- Only if both tests pass do we update the lookup table with a new entry with
-- a new node ID.
mk :: VarId -> Ref -> Ref -> State RefMap Ref
mk i l h =
  let pointee = (i, l, h)
      maxNodeId refmap = if LUT.null refmap then Nothing
                         else Just $ maximum (naturalKeys refmap)
  in if l == h then return $ l
     else do refmap <- get
             if elemB pointee refmap then return $ Ptr (refmap ¡ pointee)
             else let u = (fromMaybe (-1) $ maxNodeId refmap) + 1
                   in do put $ LUT.set u pointee refmap
                         return $ Ptr u

-- Build a reduced ordered binary decision diagram as a lookup table with an
-- initial pointer into the table for the root node.
build :: BoolExpr -> ROBDD
build expr = runState (build' maxVar Env.empty) $ LUT.empty
  where maxVar :: VarId
        maxVar = fromMaybe (-1) $ maximumVar expr
        build' :: VarId -> Env -> State RefMap Ref
        build' (-1) env = return $ Val (BE.eval expr env)
        build' i    env =
          do v₀ <- build' (i-1) (Env.set i True env)
             v₁ <- build' (i-1) (Env.set i False env)
             mk i v₀ v₁

-- Evaluate a reduced ordered binary decision diagram with an environment. Will
-- cause an error if the environment is incomplete.
eval :: ROBDD -> Env -> Bool
eval (ref, refmap) env = eval' ref
  where eval' (Val b) = b
        eval' (Ptr u) = let (i, l, h) = refmap LUT.! u
                         in eval' $ if env Env.! i then l else h

--
-- Debug:

-- Enumerate all the paths of variable IDs that can be taken down the ROBDD
-- to any of the leafs.
varPaths_ :: NodeAttr -> RefMap -> [[VarId]]
varPaths_ (i, l, h) refmap = map (i:) $
  case (l, h) of (Val _,  Val _)  -> [[]]
                 (Val _,  Ptr uₕ) -> [[]] ++ paths uₕ
                 (Ptr uₗ, Val _)  -> paths uₗ ++ [[]]
                 (Ptr uₗ, Ptr uₕ) -> paths uₗ ++ paths uₕ
  where paths :: NodeId -> [[VarId]]
        paths u = varPaths_ (refmap LUT.! u) refmap
