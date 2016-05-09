{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.ROBDD
  ( runTests
  , prop_evalROBDDEquivalency
  ) where

import Test.QuickCheck (Property, property, quickCheckAll)

import qualified BoolExpr.BoolExpr as BE
import qualified BoolExpr.ROBDD as ROBDD

import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))

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
runTests :: IO Bool
runTests = $quickCheckAll
