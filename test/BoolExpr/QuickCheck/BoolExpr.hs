{-# LANGUAGE TemplateHaskell, InstanceSigs #-}

module BoolExpr.QuickCheck.BoolExpr
  ( BoolExprWithEnv (..) -- instance Test.QuickCheck.Arbitrary
  , runTests
  , prop_trivial
  , prop_simplifyCorrectness
  , prop_partialSimplifyCorrectness
  , prop_simplifyTotalReduction
  ) where

import Control.Monad
import Test.QuickCheck

import BoolExpr.BoolExpr
import BoolExpr.Env as Env

--
-- QuickCheck arbitrary instance:

newtype BoolExprWithEnv = BEwE (BoolExpr, Env) deriving Show

-- Generate arbitrary boolean expressions with complete environments.
instance Arbitrary BoolExprWithEnv where
  arbitrary :: Gen BoolExprWithEnv
  arbitrary = do k <- choose (1, 10)
                 expr <- sized (arbBoolExpr k)
                 env <- liftM Env.mkEnv (vectorOf k arbitrary)
                 return $ BEwE (expr, env)

arbBoolExpr :: Int -> Int -> Gen BoolExpr
arbBoolExpr k 0 = frequency
  [ (8, do i <- choose (0, k - 1)
           return $ var i)
  , (1, do return $ zero)
  , (1, do return $ one)
  , (1, do e <- arbBoolExpr k 0
           return $ neg e)
  ]
arbBoolExpr k n = oneof
  [ do e₁ <- arbBoolExpr k (n `div` 2)
       e₂ <- arbBoolExpr k (n `div` 2)
       return $ e₁ ∧ e₂
  , do e₁ <- arbBoolExpr k (n `div` 2)
       e₂ <- arbBoolExpr k (n `div` 2)
       return $ e₁ ∨ e₂
  ]

--
-- Properties:

-- Evaluating a boolean expression with a complete environment results in
-- either `True` or `False`.
prop_trivial :: BoolExprWithEnv -> Property
prop_trivial (BEwE (expr, env)) =
  let res = eval expr env
   in property $ (res || not res)

-- Reducing a boolean expression may not affect the evaluation result.
prop_simplifyCorrectness :: BoolExprWithEnv -> Property
prop_simplifyCorrectness (BEwE (expr, env)) =
  property $ eval (expr `simplifyWith` env) env == eval expr env

-- Partially reducing a boolean expression may not affect the evaluation
-- result.
prop_partialSimplifyCorrectness :: BoolExprWithEnv -> Int -> Property
prop_partialSimplifyCorrectness (BEwE (expr, env)) partialSize =
  let mapT f (a0, a1) = (f a0, f a1)
      (env0, env1) = mapT fromList $ splitAt (abs partialSize `mod` size env) (assocs env)
   in property $ eval (expr `simplifyWith` env0) env1 == eval expr env

-- Reducing a boolean expression with a complete environment results in a
-- totally reduced expression (either `one` or `zero`).
prop_simplifyTotalReduction :: BoolExprWithEnv -> Property
prop_simplifyTotalReduction (BEwE (expr, env)) =
  let simplified = expr `simplifyWith` env
   in property $ simplified == one || simplified == zero

-- Run all quickcheck properties.
return []
runTests = $quickCheckAll
