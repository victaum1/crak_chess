module Main (main) where
import Test.HUnit
import qualified System.Exit as Exit

tests = TestList [TestCase $ assertEqual "0 debe ser igual a 0" 0 0]

main :: IO ()
main = do
         count <- runTestTT tests
         if failures count > 0 then Exit.exitFailure else return ()
