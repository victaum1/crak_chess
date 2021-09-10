module Main (main) where

import qualified System.Exit as Exit
import Data.Maybe
import Test.HUnit
import Control.Monad(when)
import Moves
import Utils
import Parsing

fixtures = "tests/fixtures/"
inputs   = "make_move_input.csv"

genMoves = (fst <$>) . parse pMoveCoord

noHeadLines = drop 1 . lines

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

genTest = zipWith assertEq

main :: IO ()
main = do
         inputs <- readFile(fixtures++inputs)
         let slines = noHeadLines inputs
         let triples = map (splitOn ';') slines
         let inp_strs = map head triples
         let moves = mapMaybe (genMoves . head) triples
         let spec_strs = map show moves
         let pre_tests = genTest spec_strs inp_strs
         let tests = TestList $ map TestCase pre_tests
         count <- runTestTT tests
         when (failures count > 0) Exit.exitFailure
