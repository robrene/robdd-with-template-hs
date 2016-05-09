{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.ROBDD_TH
  ( runTests
  , prop_evalCompilationEquivalency
  ) where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Test.QuickCheck (Property, property, quickCheckAll)
import Test.QuickCheck.Monadic (PropertyM, run, monadicIO)

import qualified BoolExpr.BoolExpr as BE
import qualified BoolExpr.Env as Env
import BoolExpr.ROBDD_TH (compile)

import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))

--
-- Basic properties:

-- Running ROBDDs compiled to TH from generated boolean expressions must return
-- the same result as evaluating that expression directly.
prop_evalCompilationEquivalency :: BoolExprWithEnv -> Property
prop_evalCompilationEquivalency = monadicIO . propM
  where propM :: BoolExprWithEnv -> PropertyM IO Bool
        propM = run . runQ . propQ
        propQ :: BoolExprWithEnv -> Q Bool
        propQ (BEwE (expr, env)) = let env0 = Env.mkEnv $ take (1 + fromMaybe (-1) (BE.maximumVar expr)) (Env.elems env)
                                       call = foldl appE (compile expr) $ map (\b -> [| b |]) (Env.elems env0)
                                       res = BE.eval expr env0
                                    in (liftM2 (==)) (runQ call) (lift res)

--
-- Run all quickcheck properties.
return []
runTests :: IO Bool
runTests = $quickCheckAll
