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
    , xor
    , (≡)
    , maximumVar
    , eval
    , simplify
    , simplifyWith
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

xor :: BoolExpr -> BoolExpr -> BoolExpr
xor e₁ e₂ = (e₁ ∨ e₂) ∧ (neg $ e₁ ∧ e₂)

(≡) :: BoolExpr -> BoolExpr -> BoolExpr
(≡) e₁ e₂ = neg $ e₁ `xor` e₂

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

-- Apply some basic simplification rules to a boolean expression.
simplify :: BoolExpr -> BoolExpr
simplify expr = expr `simplifyWith` empty

-- Apply some basic simplification rules to a boolean expression with an
-- environment.
simplifyWith :: BoolExpr -> Env -> BoolExpr
simplifyWith expr env = simplify' expr
  where simplify' (Var i)
          | i `member` env = lit $ env ! i
        simplify' (Not (Not e)) = simplify' e
        simplify' (Not e)
          | simplify' e == one  = zero
          | simplify' e == zero = one
          | otherwise           = neg $ simplify' e
        simplify' (And e₁ e₂)
          | simplify' e₁ == one  = simplify' e₂
          | simplify' e₂ == one  = simplify' e₁
          | simplify' e₁ == zero = zero
          | simplify' e₂ == zero = zero
          | otherwise            = simplify' e₁ ∧ simplify' e₂
        simplify' (Or e₁ e₂)
          | simplify' e₁ == one  = one
          | simplify' e₂ == one  = one
          | simplify' e₁ == zero = simplify' e₂
          | simplify' e₂ == zero = simplify' e₁
          | otherwise            = simplify' e₁ ∨ simplify' e₂
        simplify' e = e
