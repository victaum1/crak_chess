module Main (main) where
import Moves
import qualified System.Exit as Exit
import Data.Maybe
import Test.HUnit
import Control.Monad(when)


-- Game of the century: Donald Byrne vs Robert Fischer, 1956
input_str_moves = ["g1f3","g8f6","c2c4","g7g6","b1c3","f8g7","d2d4","e8g8"
  ,"c1f4","d1b3","d5c4","b3c4","c7c6","e2e4","b8d7","a1d1","d7b6","c4c5","c8g4"
  ,"f4g5","b6a4","c5a3","a4c3","a2b3","f6e4","g5e7","d8b6","f1c4","e4c3","e7c5"
  ,"f8e8","e1f1","g4e6","c5b6","e6c4","f1g1","b3e2","g1f1","e2d4","f1g1","d4e2"
  ,"g1f1","e2c3","f1g1","a7b6","a3b4","a8a4","b4b6","c3d1","h2h3","a4a2","g1h2"
  ,"d1f2","h1e1","e8e1","b6d8","g7f8","f3e1","c4d5","e1f3","f2e4","d8b8","b7b5"
  ,"h3h4","h7h5","f3e5","g8g7","h2g1","f8c5","g1f1","e4g3","f1e1","c5b4","e1d1"
  ,"d5b3","d1c1","f3d2","c1b1","d2c3","b1c1","a2b2"]

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

inputs = map (fromJust . readMove) input_str_moves

inputC = [ fst tc (snd tc) | tc <- zip inputA inputB]
  where inputA = map assertEq input_str_moves
        inputB = map show inputs

tests = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests
         when (failures count > 0) Exit.exitFailure
