import qualified BoolExpr.UnitTests.BoolExpr
import qualified BoolExpr.UnitTests.Compiler
import qualified BoolExpr.QuickCheck.BoolExpr
import qualified BoolExpr.QuickCheck.Compiler

main :: IO ()
main =
  do putStrLn ""
     putStrLn "BoolExpr unit tests:"
     BoolExpr.UnitTests.BoolExpr.runTests
     putStrLn "Compiler unit tests:"
     BoolExpr.UnitTests.Compiler.runTests
     putStrLn ""
     BoolExpr.QuickCheck.BoolExpr.runTests
     BoolExpr.QuickCheck.Compiler.runTests
     return ()
