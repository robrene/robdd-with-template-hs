{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.ROBDD_TH
  ( compile
  , compileE
  ) where

import Data.Bimap (assocs)
import Language.Haskell.TH

import BoolExpr.BoolExpr (BoolExpr, numVars)
import BoolExpr.Env (VarId)
import BoolExpr.ROBDD (NodeId, Ref (..), NodeAttr)
import qualified BoolExpr.ROBDD as ROBDD (build)

-- TH naming scheme for the node variables.
nodeName :: NodeId -> Name
nodeName u = mkName $ "n_" ++ (show u)

-- Map a ROBDD.Ref to either a boolean literal or a node variable.
mkRefExp :: Ref -> ExpQ
mkRefExp (Val b) = [| b |]
mkRefExp (Ptr u) = varE $ nodeName u

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
compile :: BoolExpr -> ExpQ
compile expr =
  let (ref, refmap) = ROBDD.build expr
      decs = map mkNodeDec (assocs refmap)
      letExp = letE decs $ mkRefExp ref
      exprSize = numVars expr
      varNums = take exprSize [0..]
      lamPat = map (varP . varName) $ varNums
   in if exprSize == 0 then letExp else lamE lamPat letExp
  where -- TH naming scheme for the boolean expression variables.
        varName :: VarId -> Name
        varName i = mkName $ "x_" ++ (show i)
        -- Map a node of the ROBDD to a TH declaration of the form:
        -- @{ n_3 = if x_2 then n_2 else False }
        mkNodeDec :: (NodeId, NodeAttr) -> DecQ
        mkNodeDec (u, (i, nₗ, nᵣ)) =
          let iteExp = condE (varE $ varName i) (mkRefExp nₗ) (mkRefExp nᵣ)
           in valD (varP $ nodeName u) (normalB iteExp) []

-- Compile a boolean expression into a TH expression of the form:
-- @{ \ i2b
--      -> let
--           n_5 = if i2b 3 then n_3 else n_4
--           n_4 = if i2b 2 then False else n_2
--           n_3 = if i2b 2 then n_2 else False
--           n_2 = if i2b 1 then n_0 else n_1
--           n_1 = if i2b 0 then False else True
--           n_0 = if i2b 0 then True else False
--         in n_5 }
compileE :: BoolExpr -> ExpQ
compileE expr =
  let (ref, refmap) = ROBDD.build expr
      decs = map mkNodeDec (assocs refmap)
      letExp = letE decs $ mkRefExp ref
   in lamE [varP $ mkName "i2b"] letExp
  where -- Lookup a boolean with the `i2b` function parameter.
        mkLookup :: VarId -> ExpQ
        mkLookup i = appE (varE $ mkName "i2b") (litE $ intPrimL (toInteger i))
        -- Map a node of the ROBDD to a TH declaration of the form:
        -- @{ n_3 = if i2b 2 then n_2 else False }
        mkNodeDec :: (NodeId, NodeAttr) -> DecQ
        mkNodeDec (u, (i, nₗ, nᵣ)) =
          let iteExp = condE (mkLookup i) (mkRefExp nₗ) (mkRefExp nᵣ)
           in valD (varP $ nodeName u) (normalB iteExp) []
