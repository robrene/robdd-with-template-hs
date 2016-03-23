{-# LANGUAGE TemplateHaskell, InstanceSigs #-}

module BoolExpr.QuickCheck.BoolExpr
  ( BoolExprWithEnv (..) -- instance Test.QuickCheck.Arbitrary
  , runTests
  , prop_trivial
  , prop_reduceCorrectness
  , prop_partialReduceCorrectness
  , prop_reduceTotalReduction
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
prop_reduceCorrectness :: BoolExprWithEnv -> Property
prop_reduceCorrectness (BEwE (expr, env)) =
  property $ eval (reduce expr env) env == eval expr env

-- Partially reducing a boolean expression may not affect the evaluation
-- result.
prop_partialReduceCorrectness :: BoolExprWithEnv -> Int -> Property
prop_partialReduceCorrectness (BEwE (expr, env)) partialSize =
  let mkEnvFromEnv :: Env -> Int -> Env -> Env
      mkEnvFromEnv _    0 new = new
      mkEnvFromEnv orig i new = mkEnvFromEnv orig (i-1) $ Env.set i (orig ! i) new
      partialEnv = mkEnvFromEnv env (abs partialSize `mod` size env) (Env.empty)
   in property $ eval (reduce expr partialEnv) env == eval expr env

-- Reducing a boolean expression with a complete environment results in a
-- totally reduced expression (either `one` or `zero`).
prop_reduceTotalReduction :: BoolExprWithEnv -> Property
prop_reduceTotalReduction (BEwE (expr, env)) =
  let reduced = reduce expr env
   in property $ reduced == one || reduced == zero

-- Run all quickcheck properties.
return []
runTests = $quickCheckAll
