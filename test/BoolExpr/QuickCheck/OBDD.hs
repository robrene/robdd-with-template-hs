{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.OBDD
  ( runTests
  , prop_evalTreeEquivalency ) where

import Test.QuickCheck (Property, property, quickCheckAll)

import qualified BoolExpr.BoolExpr as BE
import qualified BoolExpr.OBDD as OBDD
import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))

--
-- Properties:

-- Evaluating OBDD trees generated from boolean expressions must return the
-- same result as evaluating that expression directly.
prop_evalTreeEquivalency :: BoolExprWithEnv -> Property
prop_evalTreeEquivalency (BEwE (expr, env)) =
  property $ OBDD.eval (OBDD.mkTree expr) env == BE.eval expr env

-- Run all quickcheck properties.
return []
runTests = $quickCheckAll
