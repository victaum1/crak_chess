module Main (main) where
import Test.HUnit
import qualified System.Exit as Exit
import Pieces
import Data.Char(toLower)
import Data.Maybe(fromJust)
import Control.Monad(when)

-- showPiece and readCPiece are reverses
valid_chars = (map toLower piece_chars) ++ piece_chars

assertEq = assertEqual "Falla: " :: Char -> Char -> Assertion

inputC = [ fst tc (snd tc) | tc <- zip inputA inputB]
  where inputA = map assertEq valid_chars
        inputB = map showPiece inputs
        inputs = fromJust $ mapM readCPiece valid_chars

tests_idem = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests_idem
         when (failures count > 0) Exit.exitFailure
