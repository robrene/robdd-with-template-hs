module Main where

import Data.Array.Unboxed (elems)
import System.Environment (getArgs)
import Language.CNF.Parse.ParseDIMACS (parseFile, Clause, clauses)

import BoolExpr.BoolExpr
import BoolExpr.ROBDD

main :: IO ()
main = do args <- getArgs
          cnf <- parseFile $ head args
          putStrLn . show . build. mkBE $ either (const []) clauses cnf
          return ()

mkBE :: [Clause] -> BoolExpr
mkBE cs = foldr (∧) one $ map (mkDisj . elems) cs

mkDisj :: [Int] -> BoolExpr
mkDisj ids = foldr1 (∨) $ map mkVar ids

mkVar :: Int -> BoolExpr
mkVar i | i > 0 = var $ i - 1
mkVar i | i < 0 = neg $ var $ -i - 1
mkVar _ = undefined
