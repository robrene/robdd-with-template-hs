{-# LANGUAGE PatternSynonyms #-}

module BoolExpr.ROBDD
  ( NodeId
  , Ref
  , NodeAttr
  , RefMap
  , ROBDD
  , build
  ) where

import Control.Monad.State
import Data.Maybe

import BoolExpr.BoolExpr (BoolExpr, maximumVar, eval)
import BoolExpr.Env as Env
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

maxNodeId :: RefMap -> Maybe NodeId
maxNodeId refmap = if LUT.null refmap then Nothing
                   else Just $ maximum (naturalKeys refmap)

mk :: VarId -> Ref -> Ref -> State RefMap Ref
mk i l h =
  let pointee = (i, l, h)
  in if l == h then return $ l
     else do refmap <- get
             if elemB pointee refmap then return $ Ptr (fromJust (refmap ¡ pointee))
             else let u = (fromMaybe (-1) $ maxNodeId refmap) + 1
                   in do put $ LUT.set u pointee refmap
                         return $ Ptr u


build :: BoolExpr -> ROBDD
build expr = runState (build' maxVar Env.empty) $ LUT.empty
  where maxVar :: VarId
        maxVar = fromMaybe (-1) $ maximumVar expr
        build' :: VarId -> Env -> State RefMap Ref
        build' (-1) env = return $ Val (eval expr env)
        build' i    env =
          do v₀ <- build' (i-1) (Env.set i True env)
             v₁ <- build' (i-1) (Env.set i False env)
             mk i v₀ v₁
