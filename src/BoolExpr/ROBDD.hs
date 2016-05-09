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
  ) where

import Control.Monad.State
import qualified Data.Bimap as Map
import Data.Maybe

import BoolExpr.BoolExpr (BoolExpr, maximumVar)
import qualified BoolExpr.BoolExpr as BE (eval)
import BoolExpr.Env (Env, VarId)
import qualified BoolExpr.Env as Env (empty, set, (!))

--
-- Data structure:

type NodeId = Int
data Ref = Val Bool | Ptr NodeId deriving (Eq, Show, Ord)
type NodeAttr = (VarId, Ref, Ref)
type RefMap = Map.Bimap NodeId NodeAttr
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
      nextNodeId refmap = if Map.null refmap then 0
                          else 1 + fst (Map.findMax refmap)
  in if l == h then return $ l
     else do refmap <- get
             if Map.memberR pointee refmap then return $ Ptr (refmap Map.!> pointee)
             else let u = nextNodeId refmap
                   in do put $ Map.insert u pointee refmap
                         return $ Ptr u

-- Build a reduced ordered binary decision diagram as a lookup table with an
-- initial pointer into the table for the root node.
build :: BoolExpr -> ROBDD
build expr = runState (build' maxVar Env.empty) $ Map.empty
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
        eval' (Ptr u) = let (i, l, h) = refmap Map.! u
                         in eval' $ if env Env.! i then l else h
