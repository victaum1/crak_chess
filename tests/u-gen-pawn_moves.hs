module Main (main) where

import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified System.Exit as Exit
import Test.HUnit
import Data.List (sort)
import Game
import Moves (Move, readMove, pMoveCoord)
import Generator
import Squares
import Parsing

fixtures = "tests/fixtures/"
fens = "pawn_move-fens.txt"
squares = "pawn_move-sqs.txt"
specs = "pawn_move-specs.txt"

manyPMove = parse (many pMoveCoord)

assertEq = assertEqual "falla: " :: [Move] -> [Move] -> Assertion

genGames = map (fromJust . fen2Game)

genTestSqs = map (fromJust . readSquare)

genMoves = flip genAllPawnMoves

genSpecMoves = map ((fst . head) . manyPMove) . lines

genTests = zipWith assertEq

main :: IO ()
main = do
  fen_file <- readFile $ fixtures ++ fens
  let games = genGames (lines fen_file)
  let wgame = head games
  let bgame = games!!1
  sqs_file <- readFile $ fixtures ++ squares
  let test_sqs = words sqs_file
  let sqs_test_w = genTestSqs (take 8 test_sqs)
  let sqs_test_b = genTestSqs (drop 8 test_sqs)
  move_file <- readFile $ fixtures ++ specs
  let move_specs = genSpecMoves move_file
  let w_move_specs =  map sort $ take 8 move_specs
  let b_move_specs =  map sort $ drop 8 move_specs
  let w_gen_moves  =  map (sort . genMoves wgame) sqs_test_w
  let b_gen_moves  =  map (sort . genMoves bgame) sqs_test_b
  let ws = genTests w_move_specs w_gen_moves
  let bs = genTests b_move_specs b_gen_moves
  let as = ws ++ bs
  let tests = TestList $ map TestCase as
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure 
