module Main(main) where

import Data.Either
import qualified System.Exit as Exit
import Control.Monad
import Test.HUnit
import Perft
import Game
import Parsing
import Utils

fixtures = "tests/fixtures/"
inputs = "perft_data.csv"

genGame     = myRight . parse pGame ""

noHeadLine = drop 1 . lines

assertEq    = assertEqual "falla: " :: Int -> Int -> Assertion

genTest     = zipWith assertEq

genOutNodes = zipWith perft

main =
  do
    inputs_ <- readFile(fixtures++inputs)
    let slines = noHeadLine inputs_
    let triples = map (splitOn ';') slines
    let in_pos = map (genGame . head) triples
    let in_deps = map (read . (!!1)) triples
    let spec_nodes = map (read . (!!2)) triples
    let out_nodes = genOutNodes in_pos in_deps
    let pre_tests = genTest spec_nodes out_nodes
    let tests = TestList $ map TestCase pre_tests
    count <- runTestTT tests
    when (failures count > 0) Exit.exitFailure

