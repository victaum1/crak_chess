module Main (main) where

import Data.Either
import Data.Maybe
import Control.Monad
import qualified System.Exit as Exit
import Test.HUnit
import Game
import Moves
import Play
import Parsing
import Utils (splitOn)

fixtures = "tests/fixtures/"
inputs = "make_move_input.csv"

genGames = (either (\a -> Nothing) (\b -> Just b)) . parse pGame ""

genMoves = (either (\a -> Nothing) (\b -> Just b)) . parse pMoveCoord ""

noHeadLines = drop 1 . lines

genOutGames = zipWith makeMove

assertEq    = assertEqual "falla: " :: Maybe Game -> Maybe Game
  -> Assertion

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

