{-# LANGUAGE PatternSynonyms #-}

module BoolExpr.BoolExpr
    ( BoolExpr
    , pattern Var
    , pattern Zero
    , pattern One
    , pattern Not
    , pattern And
    , pattern Or
    , var
    , zero
    , one
    , lit
    , neg
    , (∧)
    , (∨)
    , maximumVar
    , eval
    , reduce
    ) where

import BoolExpr.Env

--
-- Data structure:

-- Simple and minimal boolean expression definition.
data BoolExpr =
    BEVar VarId
  | BEZero
  | BEOne
  | BENot BoolExpr
  | BEAnd BoolExpr BoolExpr
  | BEOr BoolExpr BoolExpr
  deriving (Show, Eq)

pattern Var       :: VarId -> BoolExpr
pattern Var i     <- BEVar i
pattern Zero      :: BoolExpr
pattern Zero      <- BEZero
pattern One       :: BoolExpr
pattern One       <- BEOne
pattern Not       :: BoolExpr -> BoolExpr
pattern Not e     <- BENot e
pattern And       :: BoolExpr -> BoolExpr -> BoolExpr
pattern And e₁ e₂ <- BEAnd e₁ e₂
pattern Or        :: BoolExpr -> BoolExpr -> BoolExpr
pattern Or e₁ e₂  <- BEOr e₁ e₂

var :: VarId -> BoolExpr
var i | i < 0 = error "Can only use natural numbers as variable numbers"
var i = BEVar i

zero :: BoolExpr
zero = BEZero

one :: BoolExpr
one = BEOne

lit :: Bool -> BoolExpr
lit b = if b then one else zero

neg :: BoolExpr -> BoolExpr
neg e = BENot e

(∧) :: BoolExpr -> BoolExpr -> BoolExpr
(∧) e₁ e₂ = BEAnd e₁ e₂

(∨) :: BoolExpr -> BoolExpr -> BoolExpr
(∨) e₁ e₂ = BEOr e₁ e₂

--
-- Functions:

-- Return the highest variable number inside a boolean expression, or `Nothing`
-- if there are no numbered variables inside the expression.
maximumVar :: BoolExpr -> Maybe VarId
maximumVar (Var i)     = Just i
maximumVar (Zero)      = Nothing
maximumVar (One)       = Nothing
maximumVar (Not e)     = maximumVar e
maximumVar (And e₁ e₂) = max (maximumVar e₁) (maximumVar e₂)
maximumVar (Or e₁ e₂)  = max (maximumVar e₁) (maximumVar e₂)
maximumVar _ = error "???"

-- Evaluate a boolean expression with an environment. Will cause an error
-- if the environment is incomplete.
eval :: BoolExpr -> Env -> Bool
eval expr env = case expr of Var i     -> env ! i
                             Zero      -> False
                             One       -> True
                             Not e     -> not (eval e env)
                             And e₁ e₂ -> (eval e₁ env) && (eval e₂ env)
                             Or e₁ e₂  -> (eval e₁ env) || (eval e₂ env)
                             _ -> error "???"

-- Apply some basic simple reduction rules to a boolean expression.
reduce :: BoolExpr -> Env -> BoolExpr
reduce expr env = reduce' expr
  where reduce' (Var i)
          | member i env = lit $ env ! i
        reduce' (Not e)
          | reduce' e == one  = zero
          | reduce' e == zero = one
        reduce' (And e₁ e₂)
          | reduce' e₁ == one && reduce' e₂ == one   = one
          | reduce' e₁ == one && reduce' e₂ == zero  = zero
          | reduce' e₁ == zero && reduce' e₂ == one  = zero
          | reduce' e₁ == zero && reduce' e₂ == zero = zero
        reduce' (Or e₁ e₂)
          | reduce' e₁ == one && reduce' e₂ == one   = one
          | reduce' e₁ == one && reduce' e₂ == zero  = one
          | reduce' e₁ == zero && reduce' e₂ == one  = one
          | reduce' e₁ == zero && reduce' e₂ == zero = zero
        reduce' e = e
