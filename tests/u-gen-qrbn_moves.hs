module Main (main) where

import Control.Monad
import Data.Maybe
import qualified System.Exit as Exit
import Test.HUnit
import Data.List
import Game
import Moves
import Generator
import Squares
import Parsing
import Board
import Utils

gen_funcs = concat $ map (replicate 3) [
    genKnightMoves, genBishopMoves, genRookMoves, genQueenMoves
  ]

toMaybe = either (const Nothing) Just

manyPMove = toMaybe . parse (many (many space>>pMoveCoord)) ""
manyPBoard = map (myRight . parse pFenBoard "") . words
manyPSquare = myRight . parse (many (many space>>pSquare)) ""

assertEq = assertEqual "falla: " :: Move -> Move -> Assertion

genBoards = manyPBoard

genTestSqs = manyPSquare

genMoves :: [Square] -> [Board] -> [[Move]]
genMoves = zipWith id . zipWith id gen_funcs

genSpecMoves = mapMaybe manyPMove

genTest = (zipWith . zipWith) assertEq

main:: IO ()
main = do
  move_specs_file <- readFile "tests/fixtures/nbrq_move_specs.txt"
  let mslines = drop 2 $ lines move_specs_file
  let move_specs = map sort $ genSpecMoves mslines
  move_inputs_file <- readFile "tests/fixtures/nbrq_move_inputs.txt"
  let qline = lines move_inputs_file !! 2
  let squares = genTestSqs qline
  let bline = lines move_inputs_file !! 3
  let boards = genBoards bline
  let gen_moves = map sort $ genMoves squares boards
  let pre_tests = concat $ genTest move_specs gen_moves
  let tests = TestList $ map TestCase pre_tests
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure
