module Main (main) where
import Squares
import Test.HUnit
import qualified System.Exit as Exit
import Data.Maybe(fromJust)
import Control.Monad(when)

-- showSquare and readSquare are reverses
valid_square_list = [a:b:[] | a <- file_chrs, b <- rank_chrs]

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

inputC = [ fst tc (snd tc) | tc <- zip inputA inputB]
  where inputA = map assertEq valid_square_list
        inputB = map show inputs
        inputs = fromJust $ mapM readSquare valid_square_list

tests_idem = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests_idem
         when (failures count > 0) Exit.exitFailure

