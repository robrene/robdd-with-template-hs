module Main where

import Language.Haskell.TH

import App.Benchmarks
import App.CNF2BoolExpr
import BoolExpr.BoolExpr
import BoolExpr.Env
import BoolExpr.ROBDD as ROBDD
import BoolExpr.ROBDD_TH as ROBDD_TH
import Data.Meta

mkMeta :: BoolExpr -> IO (Meta ((Int -> Bool) -> Bool))
mkMeta expr = do
  th <- runQ $ ROBDD_TH.compileE expr
  return $ Meta (filter ((/=) '#') (pprint th))

main :: IO ()
main = do expr <- cnfFile "/Users/rob/Desktop/SATLIB/uf20-91/uf20-01.cnf"
          let robdd = ROBDD.build expr
          meta <- mkMeta expr
          result <- compileMeta ["GHC.Types"] meta
          case result of
            Left err -> putStrLn "Failed: " >> putStrLn err
            Right fn -> do
              runBenchmarks expr robdd (\env -> (fn $ (!) env))
