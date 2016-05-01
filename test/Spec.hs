import qualified BoolExpr.UnitTests.BoolExpr
import qualified BoolExpr.UnitTests.Compiler
import qualified BoolExpr.QuickCheck.BoolExpr
import qualified BoolExpr.QuickCheck.Compiler
import qualified BoolExpr.QuickCheck.OBDD
import qualified BoolExpr.QuickCheck.ROBDD

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
     BoolExpr.QuickCheck.OBDD.runTests
     BoolExpr.QuickCheck.ROBDD.runTests
     return ()
