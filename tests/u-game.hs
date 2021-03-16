module Main (main) where
import Data.Maybe
import Test.HUnit
import Game
import qualified System.Exit as Exit
import Control.Monad(when)

input_fens = [
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
  "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
  "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
  "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3",
  "r1bqkbnr/ppp2ppp/2np4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4",
  "r1bqkbnr/ppp2ppp/2np4/1B2p3/3PP3/5N2/PPP2PPP/RNBQK2R b KQkq - 0 4",
  "r1bqkbnr/ppp2ppp/2np4/1B6/3pP3/5N2/PPP2PPP/RNBQK2R w KQkq - 0 5",
  "r1bqkbnr/ppp2ppp/2np4/1B6/3NP3/8/PPP2PPP/RNBQK2R b KQkq - 0 5",
  "r2qkbnr/pppb1ppp/2np4/1B6/3NP3/8/PPP2PPP/RNBQK2R w KQkq - 1 6",
  "r2qkbnr/pppb1ppp/2Np4/1B6/4P3/8/PPP2PPP/RNBQK2R b KQkq - 0 6"
  ,"1r2r1k1/p1p1qpp1/bb1p1n1p/2p1p3/4P3/2NPBNQ1/PPP2PPP/R4RK1 b - - 1 8"
  ,"1r2r3/p1p1qppk/bb1p1n1p/2p1p3/4P3/3PBNQ1/PPP1NPPP/R4RK1 b - - 3 9"
  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/3PBNQP/PPP1NPP1/R4RK1 b - - 0 1"
  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R4RK1 b - - 0 2"
  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R4RK1 b - - 0 2"
  ,"1r2r3/p1b1qppk/b1pp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R3R1K1 b - - 2 3"
  ,"1r2r3/p1b1qppk/b1p2n1p/2ppp3/4P2P/2PPBNQ1/PP2NPP1/R3R1K1 b - h3 0 4"
  ,"1r2r3/p1b1qp1k/b1p2npp/2ppp3/4P2P/2PPBNQ1/PP2NPP1/R3R1K1 w - - 0 2"
  ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3pP2P/2PPBNQ1/PP2NPP1/R3R1K1 w - - 0 5"
  ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3pP2P/2PP1NQ1/PP1BNPP1/R3R1K1 b - - 1 5"
  ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3PP2P/5NQ1/PP1BNPP1/R3R1K1 w - - 1 7"
  ,"1r2r3/p3qppk/bbp2n1p/2p1p3/3PP2P/1P3NQ1/P2BNPP1/R3R1K1 w - - 1 2"
  ,"1r2r3/p1b1qppk/b1p2n1p/4p3/1p1PP2P/5N1Q/P2BNPP1/R3R1K1 b - - 1 1"
  ,"1r2r3/p1b1qppk/b1p2n1p/4P3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 b - - 0 8"
  ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 2"
  ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 9"
  ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 9"
  ,"1r2r3/p3qppk/b1p2n1p/4Q3/1p2P2P/5N2/P2BNPP1/R3R1K1 b - - 0 1"
  ,"1r2r3/p3qppk/b1p2n1p/4N3/1p2P2P/6Q1/P2BNPP1/R3R1K1 b - - 0 9"
  ]

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

inputC = [ fst tc (snd tc) | tc <- zip inputA inputB]
  where inputA = map assertEq input_fens
        inputB = map game2FEN inputs
        inputs = map (fromJust . fen2Game) input_fens

tests = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests
         when (failures count > 0) Exit.exitFailure

