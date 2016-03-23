{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.Compiler
  ( runTests
  , prop_cmpQExpEquivalency
  ) where

import Control.Monad (liftM2)
import Language.Haskell.TH (Q, runQ)
import Language.Haskell.TH.Syntax (lift)
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (PropertyM, run, monadicIO)

import BoolExpr.BoolExpr (eval)
import BoolExpr.Compiler (cmpQExp)
import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))

--
-- Properties:

-- Running compiled boolean expressions must return the same result as
-- evaluating that expression directly.
prop_cmpQExpEquivalency :: BoolExprWithEnv -> Property
prop_cmpQExpEquivalency = monadicIO . propM
  where propM :: BoolExprWithEnv -> PropertyM IO Bool
        propM = run . runQ . propQ
        propQ :: BoolExprWithEnv -> Q Bool
        propQ (BEwE (expr, env)) = let code = cmpQExp expr env
                                       res = eval expr env
                                    in (liftM2 (==)) (runQ code) (lift res)

-- Run all quickcheck properties.
return []
runTests = $quickCheckAll
