{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.QuickCheck.ROBDD_TH
  ( runTests
  , prop_evalCompilationEquivalency
  , prop_evalCompilationEEquivalency
  ) where

import Language.Haskell.TH
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import qualified BoolExpr.BoolExpr as BE
import qualified BoolExpr.Env as Env
import BoolExpr.ROBDD_TH
import Data.Meta

import BoolExpr.QuickCheck.BoolExpr (BoolExprWithEnv (..))

--
-- Basic properties:

-- Running ROBDDs compiled to TH from generated boolean expressions must return
-- the same result as evaluating that expression directly.
prop_evalCompilationEquivalency :: BoolExprWithEnv -> Property
prop_evalCompilationEquivalency (BEwE (expr, env)) = monadicIO $ do
  let env0 = Env.mkEnv $ take (BE.numVars expr) (Env.elems env)
  code <- run $ runQ (foldl appE (compile expr) $ map (\b -> [| b |]) (Env.elems env0))
  let meta = (Meta (pprint code) :: Meta Bool)
  result <- run $ compileMeta ["GHC.Types"] meta
  case result of
    Left err -> do run $ putStrLn "Failed: "
                   run $ putStrLn err
                   assert False
    Right fn -> assert (fn == BE.eval expr env)

-- Running ROBDDs compiled to TH from generated boolean expressions must return
-- the same result as evaluating that expression directly.
prop_evalCompilationEEquivalency :: BoolExprWithEnv -> Property
prop_evalCompilationEEquivalency (BEwE (expr, env)) = monadicIO $ do
  th <- run $ runQ (compileE expr)
  let meta = (Meta (filter ((/=) '#') (pprint th)) :: Meta ((Int -> Bool) -> Bool))
  result <- run $ compileMeta ["GHC.Types"] meta
  case result of
    Left err -> do run $ putStrLn "Failed: "
                   run $ putStrLn err
                   assert False
    Right fn -> assert (fn (\i -> env Env.! i) == BE.eval expr env)



--
-- Run all quickcheck properties.
return []
runTests :: IO Bool
runTests = $quickCheckAll
