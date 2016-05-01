{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.ROBDD_TH
  ( compile
  ) where

import Data.Maybe
import Language.Haskell.TH

import BoolExpr.BoolExpr (BoolExpr, maximumVar)
import BoolExpr.Env (VarId)
import BoolExpr.ROBDD (NodeId, Ref (..), NodeAttr)
import qualified BoolExpr.ROBDD as ROBDD (build)
import Util.BiDLUT (assocs)

-- TH naming scheme for the node variables.
nodeName :: NodeId -> Name
nodeName u = mkName $ "n_" ++ (show u)

-- TH naming scheme for the boolean expression variables.
varName :: VarId -> Name
varName i = mkName $ "x_" ++ (show i)

-- Map a ROBDD.Ref to either a boolean literal or a node variable.
mkRefExp :: Ref -> ExpQ
mkRefExp (Val b) = [| b |]
mkRefExp (Ptr u) = varE $ nodeName u

-- Map a node of the ROBDD to a TH declaration of the form:
-- @{ n_3 = if x_2 then n_2 else False }
mkNodeDec :: (NodeId, NodeAttr) -> DecQ
mkNodeDec (u, (i, nₗ, nᵣ)) =
  let iteExp = condE (varE $ varName i) (mkRefExp nₗ) (mkRefExp nᵣ)
   in valD (varP $ nodeName u) (normalB iteExp) []

-- Compile a boolean expression into a TH expression of the form:
-- @{ \ x_0 x_1 x_2 x_3
--      -> let
--           n_5 = if x_3 then n_3 else n_4
--           n_4 = if x_2 then False else n_2
--           n_3 = if x_2 then n_2 else False
--           n_2 = if x_1 then n_0 else n_1
--           n_1 = if x_0 then False else True
--           n_0 = if x_0 then True else False
--         in n_5 }
compile :: BoolExpr -> Q Exp
compile expr =
  let (ref, refmap) = ROBDD.build expr
      decs = map mkNodeDec (assocs refmap)
      letExp = letE decs $ mkRefExp ref
      varNums = take (1 + (fromJust $ maximumVar expr)) [0..]
      lamPat = map (varP . varName) $ varNums
   in lamE lamPat letExp
-- TODO: Experiment with reverse variable order
