module Main (main) where

import Control.Monad (when)
import qualified System.Exit as Exit
import Data.Maybe (mapMaybe)
import Test.HUnit (Assertion, Test(TestList, TestCase), runTestTT, assertEqual, failures)
import Game ( Game, pGame )
import Moves ( pMoveCoord )
import Play ( makeMove )
import Parsing ( parse )
import Utils (splitOn)

fixtures = "tests/fixtures/"
inputs = "make_move_input.csv"

genGames = (fst <$>) . parse pGame

genMoves = (fst <$>) . parse pMoveCoord

noHeadLines = drop 1 . lines

genOutGames = zipWith makeMove

assertEq    = assertEqual "falla: " :: Maybe Game -> Maybe
  Game -> Assertion

genTest     = zipWith assertEq

main:: IO ()
main =
  do
  inputs_ <- readFile(fixtures ++ inputs)
  let slines = noHeadLines inputs_
  let triples = map (splitOn ';') slines
  let moves = mapMaybe (genMoves . head) triples
  let inp_games = mapMaybe (genGames . (!!1)) triples
  let spec_games = map (genGames . (!!2)) triples
  let out_games = genOutGames moves inp_games
  let pre_tests = genTest spec_games out_games
  let tests = TestList $ map TestCase pre_tests
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure
