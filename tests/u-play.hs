module Main (main) where

import Control.Monad (when)
import qualified System.Exit as Exit
import Test.HUnit (Assertion, Test(TestList, TestCase), runTestTT, assertEqual, failures)
import Game ( Game, pGame )
import Moves ( pMoveCoord )
import Play ( makeMove )
import Parsing ( parse )

fixtures = "tests/fixtures/"
inputs = "make_move_inputs.txt"

genGames = fst . head . parse pGame

-- pNoGame :: Parser (Maybe Game)
-- pNoGame = do
--   space
--   symbol "//"
--   return Nothing

-- pMaybeGame :: Parser (Maybe Game)
-- pMaybeGame = do
--   space
--   Just <$> pGame <|> pNoGame

-- manyMayBeGames = parse (many pMaybeGame)

genMoves = fst. head . parse pMoveCoord

noHeadLines = drop 2 . lines

genOutGames = zipWith makeMove

assertEq    = assertEqual "falla: " :: Maybe Game -> Maybe
  Game -> Assertion

genTest     = zipWith assertEq

main:: IO ()
main =
  do
  inputs_ <- readFile(fixtures ++ inputs)
  let slines = noHeadLines inputs_
  let inp_games = map genGames $ take 144 slines
  let moves = map genMoves $ drop 145 slines
  let spec_games = map Just $ tail inp_games
  let out_games = genOutGames moves $ init inp_games
  let pre_tests = genTest spec_games out_games
  let tests = TestList $ map TestCase pre_tests
  count <- runTestTT tests
  when (failures count > 0) Exit.exitFailure
