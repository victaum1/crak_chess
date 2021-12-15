module Main (main) where

import Control.Monad (when)
import Data.Maybe
import Game
import Generator
import Squares
import qualified System.Exit as Exit
import Test.HUnit

assertEq = assertEqual "falla: " :: Bool -> Bool -> Assertion

genGames = mapMaybe fen2Game

genTestSqs = mapMaybe readSquare

main :: IO ()
main = do
  fen_file <- readFile "tests/fixtures/square_attack_fens.txt"
  let fENs = lines fen_file
  sqs_file <- readFile "tests/fixtures/attacked_sqs.txt"
  let test_sqs = words sqs_file
  bool_file <- readFile "tests/fixtures/attacked_spec_bools.txt"
  let spec_bools = (map read $ words bool_file)::[Bool]
  let games = genGames fENs
  let sqs_test = genTestSqs test_sqs
  let out_bools = zipWith squareAttack sqs_test games
  let inputA = map assertEq out_bools
  let inputC = [fst tc (snd tc) | tc <- zip inputA spec_bools]
  let tests = TestList $ map TestCase inputC
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure 
