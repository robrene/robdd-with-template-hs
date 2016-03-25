import qualified BoolExpr.UnitTests.BoolExpr
import qualified BoolExpr.UnitTests.Compiler
import qualified BoolExpr.QuickCheck.BoolExpr
import qualified BoolExpr.QuickCheck.Compiler
import qualified BoolExpr.QuickCheck.OBDD

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
     return ()
