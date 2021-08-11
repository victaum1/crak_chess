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
import Board

gen_funcs = concat $ map (replicate 3) [
    genKnightMoves, genBishopMoves, genRookMoves, genQueenMoves
  ]

manyPMove = parse (many pMoveCoord)
manyPBoard = parse (many pFenBoard)
manyPSquare = parse (many pSquare)

assertEq = assertEqual "falla: " :: Move -> Move -> Assertion

genBoards = map ((fst . head) . manyPBoard)

genTestSqs = map ((fst.head).manyPSquare)

genMoves :: [Square] -> [Board] -> [[Move]]
genMoves = zipWith id . zipWith id gen_funcs

genSpecMoves = map ((fst . head) . manyPMove)

genTest = (zipWith . zipWith) assertEq

main:: IO ()
main = do
  move_specs_file <- readFile "tests/fixtures/nbrq_move_specs.txt"
  let move_specs = map sort $ genSpecMoves $ drop 2 $ lines move_specs_file
  move_inputs_file <- readFile "tests/fixtures/nbrq_move_inputs.txt"
  let squares = head $ genTestSqs $ take 1 $ drop 2 $ lines move_inputs_file
  let boards = head $ genBoards $ drop 3 $ lines move_inputs_file
  let gen_moves = map sort $ genMoves squares boards
  let pre_tests = concat $ genTest move_specs gen_moves
  let tests = TestList $ map TestCase pre_tests
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure
