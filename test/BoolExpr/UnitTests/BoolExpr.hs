module BoolExpr.UnitTests.BoolExpr ( runTests ) where

import Test.HUnit

import BoolExpr.BoolExpr
import BoolExpr.Env as Env

runTests = runTestTT tests

tests = TestList
  [ testEvalZero
  , testEvalOne
  , testEvalNot
  , testEvalOneAndOne
  , testEvalOneAndZero
  , testEvalOneOrZero
  , testEvalZeroOrZero
  , testEvalXTrue
  , testEvalXFalse
  ]

--
-- Some helper functions:

evalCaseWith :: [Bool] -> Bool -> BoolExpr -> Test
evalCaseWith bs result expr = TestCase $ eval expr (Env.mkEnv bs) @=? result

evalCase :: Bool -> BoolExpr -> Test
evalCase = evalCaseWith []

--
-- Basic tests:

testEvalZero :: Test
testEvalZero = evalCase False $ zero

testEvalOne :: Test
testEvalOne = evalCase True $ one

testEvalNot :: Test
testEvalNot = evalCase False $ neg one

testEvalOneAndOne :: Test
testEvalOneAndOne = evalCase True $ one ∧ one

testEvalOneAndZero :: Test
testEvalOneAndZero = evalCase False $ one ∧ zero

testEvalOneOrZero :: Test
testEvalOneOrZero = evalCase True $ one ∨ zero

testEvalZeroOrZero :: Test
testEvalZeroOrZero = evalCase False $ zero ∨ zero

--
-- Tests using variables:

testEvalXTrue :: Test
testEvalXTrue = evalCaseWith [True] True $ var 0

testEvalXFalse :: Test
testEvalXFalse = evalCaseWith [False] False $ var 0
