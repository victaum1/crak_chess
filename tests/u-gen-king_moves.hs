module Main (main) where

import Control.Monad (when)
-- import Data.Maybe (fromJust)
import qualified System.Exit as Exit
import Test.HUnit
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Game
import Moves
import Generator
import Parsing

fixtures = "tests/fixtures/"
inputs = "king_move_inputs.txt"
specs = "king_move_specs.txt"


noHeadLines = drop 2 . lines

manyPMove = parse (many pMoveCoord)

assertEq = assertEqual "falla: " :: [Move] -> [Move] -> Assertion

genGames = mapMaybe ((fst <$>) . parse pGame)
genSpecMoves = mapMaybe ((fst <$>) . manyPMove)

genMoves :: [Game] -> [[Move]]
genMoves = map genKingMoves

genTest = zipWith assertEq

main:: IO ()
main = -- undefined
  do
  move_specs_file <- readFile (fixtures ++ specs)
  move_inputs_file <- readFile (fixtures ++ inputs)
  let games = genGames $ noHeadLines move_inputs_file
  let gen_moves = (map sort . genMoves) games
  let move_specs = (map sort . genSpecMoves . noHeadLines) move_specs_file
  let pre_tests = genTest move_specs gen_moves
  let tests = TestList $ map TestCase pre_tests
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure
