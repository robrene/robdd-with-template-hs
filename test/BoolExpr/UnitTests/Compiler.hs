{-# LANGUAGE TemplateHaskell #-}

module BoolExpr.UnitTests.Compiler ( runTests ) where

import Test.HUnit

import BoolExpr.BoolExpr
import BoolExpr.Compiler
import BoolExpr.Env as Env

runTests = runTestTT tests

tests = TestList
  [ testCmpQExpZero
  , testCmpQExpOne
  , testCmpQExpNot
  , testCmpQExpOneAndOne
  , testCmpQExpOneAndZero
  , testCmpQExpOneOrZero
  , testCmpQExpZeroOrZero
  , testCmpQExpXTrue
  , testCmpQExpXFalse
  ]

-- Basic tests

testCmpQExpZero :: Test
testCmpQExpZero = TestCase $ $(cmpQExp zero Env.empty) @=? False

testCmpQExpOne :: Test
testCmpQExpOne = TestCase $ $(cmpQExp one Env.empty) @=? True

testCmpQExpNot :: Test
testCmpQExpNot = TestCase $ $(cmpQExp (neg one) Env.empty) @=? False

testCmpQExpOneAndOne :: Test
testCmpQExpOneAndOne = TestCase $ $(cmpQExp (one ∧ one) Env.empty) @=? True

testCmpQExpOneAndZero :: Test
testCmpQExpOneAndZero = TestCase $ $(cmpQExp (one ∧ zero) Env.empty) @=? False

testCmpQExpOneOrZero :: Test
testCmpQExpOneOrZero = TestCase $ $(cmpQExp (one ∨ zero) Env.empty) @=? True

testCmpQExpZeroOrZero :: Test
testCmpQExpZeroOrZero = TestCase $ $(cmpQExp (zero ∨ zero) Env.empty) @=? False

-- Tests using variables

testCmpQExpXTrue :: Test
testCmpQExpXTrue = TestCase $ $(cmpQExp (var 0) (Env.singleton 0 True)) @=? True

testCmpQExpXFalse :: Test
testCmpQExpXFalse = TestCase $ $(cmpQExp (var 0) (Env.singleton 0 False)) @=? False
