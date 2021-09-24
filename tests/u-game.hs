module Main (main) where

import qualified System.Exit as Exit
import Control.Monad(when)
import Data.Either
import Test.HUnit
import Game
import Utils

genInputGames = map (myRight . fen2Game)
genSpecFENs   = map game2FEN
genAsserts    = map assertEq

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

main :: IO ()
main = do
         ffens <- readFile "tests/fixtures/u-g-fens.txt"
         let inp_fens = lines ffens
         let games = genInputGames inp_fens
         let inputB = genSpecFENs games
         let asserts = genAsserts inp_fens
         let inputC = [ fst tc (snd tc) | tc <- zip asserts inputB]
         let tests = TestList $ map TestCase inputC
         count <- runTestTT tests
         when (failures count > 0) Exit.exitFailure

