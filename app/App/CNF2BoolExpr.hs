module App.CNF2BoolExpr
    ( cnfFile
    , cnf2be
    ) where

import BoolExpr.BoolExpr
import Data.Array.Unboxed (elems)
import Language.CNF.Parse.ParseDIMACS

emptyCNF :: CNF
emptyCNF = CNF 0 0 []

cnfFile :: String -> IO BoolExpr
cnfFile f = do cnf <- parseFile f
               return $ cnf2be (either (const emptyCNF) id cnf)

cnf2be :: CNF -> BoolExpr
cnf2be cnf = foldr (∧) one $ map (mkDisj . elems) (clauses cnf)

mkDisj :: [Int] -> BoolExpr
mkDisj ids = foldr1 (∨) $ map mkVar ids

mkVar :: Int -> BoolExpr
mkVar i | i > 0 = var $ i - 1
mkVar i | i < 0 = neg $ var $ -i - 1
mkVar _ = error "Zero is illegal variable id in DIMACS format"
