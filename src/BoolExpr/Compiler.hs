{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.Compiler
    ( cmpQExp
    , cmpITE
    ) where

import Data.Maybe
import Language.Haskell.TH

import BoolExpr.BoolExpr
import BoolExpr.Env as Env

{-
Compile boolean expressions into Template Haskell code.
-}

-- Turn a boolean expression together with an environment into an equivalent
-- Haskell expression.
cmpQExp :: BoolExpr -> Env -> Q Exp
cmpQExp be env =
  case be of Var i     -> let res = env ! i
                           in [| res |]
             Zero      -> [| False |]
             One       -> [| True |]
             Not e     -> let inner = cmpQExp e env
                           in [| not $ $(inner) |]
             And e₁ e₂ -> let lhs = cmpQExp e₁ env
                              rhs = cmpQExp e₂ env
                           in [| $(lhs) && $(rhs) |]
             Or e₁ e₂  -> let lhs = cmpQExp e₁ env
                              rhs = cmpQExp e₂ env
                           in [| $(lhs) || $(rhs) |]
             _ -> error "???"

-- Turn a boolean expression with `n` free variables into an equivalent Haskell
-- expression with `n` parameters that are chained in an `if-then-else` style
-- construction.
cmpITE :: BoolExpr -> Q Exp
cmpITE be = cmpITE' (fromMaybe (-1) (maximumVar be)) (Env.empty)
  where cmpITE' (-1) env =
          let res = eval be env
           in [| res |]
        cmpITE' i env =
          let trueExp = cmpITE' (i-1) (Env.set i True env)
              falseExp = cmpITE' (i-1) (Env.set i False env)
              varName = "x_" ++ (show i)
           in [| if $(varE (mkName varName)) then $(trueExp) else $(falseExp) |]
