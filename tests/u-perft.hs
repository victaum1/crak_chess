module Main(main) where

import Data.Maybe
import qualified System.Exit as Exit
import Control.Monad (when)
import Test.HUnit
import Perft
import Game
import Parsing
import Utils(splitOn)

fixtures = "tests/fixtures/"
inputs = "perft_data.csv"

genGame     = (fst <$>) . parse pGame

noHeadLine = drop 1 . lines

assertEq    = assertEqual "falla: " :: Int -> Int -> Assertion

genTest     = zipWith assertEq

genOutNodes = zipWith perft

main =
  do
    inputs_ <- readFile(fixtures++inputs)
    let slines = noHeadLine inputs_
    let triples = map (splitOn ';') slines
    let in_pos = mapMaybe (genGame . head) triples
    let in_deps = map (read . (!!1)) triples
    let spec_nodes = map (read . (!!2)) triples
    let out_nodes = genOutNodes in_pos in_deps
    let pre_tests = genTest spec_nodes out_nodes
    let tests = TestList $ map TestCase pre_tests
    count <- runTestTT tests
    when (failures count > 0) Exit.exitFailure

