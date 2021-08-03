module Main (main) where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Game
import Moves (Move, readMove, pMoveCoord)
import Generator
import Squares
import qualified System.Exit as Exit
import Test.HUnit
import Parsing

manyPMove = parse (many pMoveCoord)

assertEq = assertEqual "falla: " :: Move -> Move -> Assertion

genGames = map (fromJust . fen2Game)

genTestSqs = map (fromJust . readSquare)

genMoves = flip genAllPawnMoves


main :: IO ()
main = do
  fen_file <- readFile "tests/fixtures/pawn_move-fens.txt"
  let games = genGames (lines fen_file)
  let wgame = head games
  let bgame = games!!1
  sqs_file <- readFile "tests/fixtures/pawn_move-sqs.txt"
  let test_sqs = words sqs_file
  let sqs_test_w = genTestSqs (take 8 test_sqs)
  let sqs_test_b = genTestSqs (drop 8 test_sqs)
  move_file <- readFile "tests/fixtures/pawn_move-specs.txt"
  let move_specs = map ((fst . head) . manyPMove) (lines move_file)
  let w_move_specs =  take 8 move_specs
  let b_move_specs = drop 8 move_specs
  let w_gen_moves = map (genMoves wgame) sqs_test_w
  let b_gen_moves = map (genMoves bgame) sqs_test_b
  let ws = zipWith assertEq (concat w_move_specs) (concat w_gen_moves)
  let bs = zipWith assertEq (concat b_move_specs) (concat b_gen_moves)
  let as = ws ++ bs
  let tests = TestList $ map TestCase as
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure 
