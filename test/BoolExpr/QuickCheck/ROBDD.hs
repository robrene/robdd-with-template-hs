{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.ROBDD
  ( runTests
  , prop_evalROBDDEquivalency
  ) where

import Data.Maybe (fromJust)
import Test.QuickCheck (Property, property, quickCheckAll)

import qualified BoolExpr.BoolExpr as BE
import qualified BoolExpr.ROBDD as ROBDD

import BoolExpr.Env (VarId)
import BoolExpr.ROBDD (ROBDD, NodeId, Ref (..), NodeAttr)
import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))
import Util.BiDLUT ((!))

--
-- Basic properties:

-- Evaluating ROBDDs generated from boolean expressions must return the same
-- result as evaluating that expression directly.
prop_evalROBDDEquivalency :: BoolExprWithEnv -> Property
prop_evalROBDDEquivalency (BEwE (expr, env)) =
  property $ ROBDD.eval (ROBDD.build expr) env == BE.eval expr env


--
-- Run all quickcheck properties.
return []
runTests = $quickCheckAll
