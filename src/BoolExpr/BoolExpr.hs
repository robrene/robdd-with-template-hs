module BoolExpr.BoolExpr
    ( BoolExpr (..)
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
    , numVars
    , eval
    , simplify
    , simplifyWith
    ) where

import BoolExpr.Env

--
-- Data structure:

-- Simple and minimal boolean expression definition.
data BoolExpr =
    Var VarId
  | Zero
  | One
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  deriving (Show, Eq)

var :: VarId -> BoolExpr
var i | i < 0 = error "Can only use natural numbers as variable numbers"
var i = Var i

zero :: BoolExpr
zero = Zero

one :: BoolExpr
one = One

lit :: Bool -> BoolExpr
lit b = if b then one else zero

neg :: BoolExpr -> BoolExpr
neg e = Not e

(∧) :: BoolExpr -> BoolExpr -> BoolExpr
(∧) e₁ e₂ = And e₁ e₂

(∨) :: BoolExpr -> BoolExpr -> BoolExpr
(∨) e₁ e₂ = Or e₁ e₂

xor :: BoolExpr -> BoolExpr -> BoolExpr
xor e₁ e₂ = (e₁ ∨ e₂) ∧ (neg $ e₁ ∧ e₂)

(≡) :: BoolExpr -> BoolExpr -> BoolExpr
(≡) e₁ e₂ = neg (e₁ `xor` e₂)

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

-- Return the number of variables inside a boolean expression based on the
-- highest variable number.
numVars :: BoolExpr -> Int
numVars expr = numVars' $ maximumVar expr
  where numVars' Nothing  = 0
        numVars' (Just i) = i + 1

-- Evaluate a boolean expression with an environment. Will cause an error
-- if the environment is incomplete.
eval :: BoolExpr -> Env -> Bool
eval expr env = case expr of Var i     -> env ! i
                             Zero      -> False
                             One       -> True
                             Not e     -> not (eval e env)
                             And e₁ e₂ -> (eval e₁ env) && (eval e₂ env)
                             Or e₁ e₂  -> (eval e₁ env) || (eval e₂ env)

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
