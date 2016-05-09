{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module App.Benchmarks where

import Criterion.Main as Crit
import Data.Maybe

import BoolExpr.BoolExpr as BE
import BoolExpr.Env as Env
import BoolExpr.ROBDD as ROBDD

runBenchmarks :: BoolExpr -> ROBDD -> (Env -> Bool) -> IO ()
runBenchmarks !expr !robdd !fn  =
  robdd `seq` Crit.defaultMain $ map mkBenchGroup envs
  where envs = mkEnvs $ 1 + (fromMaybe (-1) $ BE.maximumVar expr)
        mkBenchGroup (n, e) = Crit.bgroup n [ evalBenchmark expr e
                                            , robddBenchmark robdd e
                                            , thBenchmark fn e
                                            ]

mkEnvs :: Int -> [(String, Env)]
mkEnvs m = zip (map show ([0..] :: [Int])) (map Env.mkEnv $ bs m)
  where bs 0 = [[]]
        bs i = map (False:) (bs (i-1)) ++ map (True:) (bs (i-1))

evalBenchmark :: BoolExpr -> Env -> Benchmark
evalBenchmark !expr !e = Crit.bench "eval" $ whnf (BE.eval expr) e

robddBenchmark :: ROBDD -> Env -> Benchmark
robddBenchmark !robdd !e = Crit.bench "robdd" $ whnf (ROBDD.eval robdd) e

thBenchmark :: (Env -> Bool) -> Env -> Benchmark
thBenchmark !fn !e = Crit.bench "th" $ whnf fn e
