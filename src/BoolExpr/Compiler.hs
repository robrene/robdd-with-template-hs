{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.Compiler
    ( cmpQExp
    ) where

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

