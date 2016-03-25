{-# LANGUAGE PatternSynonyms #-}

module BoolExpr.OBDD
  ( OBDD
  , pattern Zero
  , pattern One
  , pattern Node
  , mkTree
  , eval
  ) where

import Data.Maybe

import BoolExpr.BoolExpr (BoolExpr, maximumVar)
import qualified BoolExpr.BoolExpr as BE (eval)
import BoolExpr.Env as Env

--
-- Data structure:

-- Ordered binary decision diagram.
data OBDD =
    OBDDZero                -- Represents a `False` leaf
  | OBDDOne                 -- Represents a `True` leaf
  | OBDDNode OBDD Int OBDD  -- Represents the split on a variable with
                            -- identifier `i`.
                            -- The left sub tree corresponds to `t[1/x_i]` and
                            -- the right subtree corresponds to `t[0/x_1]`
  deriving (Show, Eq)

pattern Zero              :: OBDD
pattern Zero              <- OBDDZero
pattern One               :: OBDD
pattern One               <- OBDDOne
pattern Node              :: OBDD -> Int -> OBDD -> OBDD
pattern Node left i right <- OBDDNode left i right

zero :: OBDD
zero = OBDDZero

one :: OBDD
one = OBDDOne

node :: OBDD -> Int -> OBDD -> OBDD
node left i right = OBDDNode left i right

--
-- Construction:

-- Create the trivial ordered binary decision diagram with the worst case size
-- which represents a boolean expression as a decision tree.
mkTree :: BoolExpr -> OBDD
mkTree expr = buildTree (fromMaybe (-1) (maximumVar expr)) (Env.empty)
  where buildTree :: Int -> Env -> OBDD
        buildTree (-1) env = if BE.eval expr env then one else zero
        buildTree i    env =
          let left  = buildTree (i-1) (Env.set i True env)
              right = buildTree (i-1) (Env.set i False env)
           in node left i right

-- Evaluate an ordered boolean decision diagram with an environment. Will cause
-- an error if the environment is incomplete.
eval :: OBDD -> Env -> Bool
eval obdd env =
  case obdd of Zero              -> False
               One               -> True
               Node left i right -> let child = if env ! i then left else right
                                     in eval child env
               _ -> error "???"
