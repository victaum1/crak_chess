module Main (main) where
import Squares
import Game
import Generator
import qualified System.Exit as Exit
import Data.Maybe
import Test.HUnit

fENs = [
  "r1bqkbnr/pppppppp/2n5/8/2B1P3/8/PPPP1PPP/RNBQK1NR b KQkq - 2 2"
  ,"r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
  ,"r1bqkbnr/ppp2ppp/2np4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 2"
  ,"r1bqk2r/ppppbppp/2n2n2/1B2p1N1/4P3/8/PPPP1PPP/RNBQK2R b KQkq - 7 5"
  ,"r2qr1k1/p1ppbppp/b1p2n2/4p1N1/4P3/3P2Q1/PPP2PPP/RNB1K2R b KQ - 4 9"
  ,"r2qr1k1/p1pp1ppp/b1p2n2/2b1p1N1/4P3/3P2Q1/PPP2PPP/RNB1K2R w KQ - 5 2"
  ,"r2qr1k1/p1pp1ppp/bbp2n2/4p1N1/4P3/2NP2Q1/PPP2PPP/R1B1K2R w KQ - 7 3"
  ,"r2qr1k1/p1pp1pp1/bbp2n1p/4p1N1/N3P3/3P2Q1/PPP2PPP/R1B1K2R w KQ - 0 4"
  ,"r2qr1k1/p1pp1pp1/bbp2n1p/4p3/N3P3/3P1NQ1/PPP2PPP/R1B1K2R b KQ - 1 4"
  ,"1r1qr1k1/p1pp1pp1/bbp2n1p/4p3/N3P3/3P1NQ1/PPPB1PPP/R3K2R b KQ - 3 5"
  ,"1r2r1k1/p1p1qpp1/bbpp1n1p/4p3/4P3/2NPBNQ1/PPP2PPP/R3K2R b KQ - 1 7"
--  ,"1r2r1k1/p1p1qpp1/bb1p1n1p/2p1p3/4P3/2NPBNQ1/PPP2PPP/R4RK1 b - - 1 8"
--  ,"1r2r3/p1p1qppk/bb1p1n1p/2p1p3/4P3/3PBNQ1/PPP1NPPP/R4RK1 b - - 3 9"
--  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/3PBNQP/PPP1NPP1/R4RK1 b - - 0 1"
--  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R4RK1 b - - 0 2"
--  ,"1r2r3/p3qppk/bbpp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R4RK1 b - - 0 2"
--   ,"1r2r3/p1b1qppk/b1pp1n1p/2p1p3/4P3/2PPBNQ1/PP2NPPP/R3R1K1 b - - 2 3"
--   ,"1r2r3/p1b1qppk/b1p2n1p/2ppp3/4P2P/2PPBNQ1/PP2NPP1/R3R1K1 b - h3 0 4"
--   ,"1r2r3/p1b1qp1k/b1p2npp/2ppp3/4P2P/2PPBNQ1/PP2NPP1/R3R1K1 w - - 0 2"
--   ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3pP2P/2PPBNQ1/PP2NPP1/R3R1K1 w - - 0 5"
--   ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3pP2P/2PP1NQ1/PP1BNPP1/R3R1K1 b - - 1 5"
--   ,"1r2r3/p1b1qppk/b1p2n1p/2p1p3/3PP2P/5NQ1/PP1BNPP1/R3R1K1 w - - 1 7"
--   ,"1r2r3/p3qppk/bbp2n1p/2p1p3/3PP2P/1P3NQ1/P2BNPP1/R3R1K1 w - - 1 2"
--   ,"1r2r3/p1b1qppk/b1p2n1p/4p3/1p1PP2P/5N1Q/P2BNPP1/R3R1K1 b - - 1 1"
--   ,"1r2r3/p1b1qppk/b1p2n1p/4P3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 b - - 0 8"
--   ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 2"
--   ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 9"
--   ,"1r2r3/p3qppk/b1p2n1p/4b3/1p2P2P/5NQ1/P2BNPP1/R3R1K1 w - - 0 9"
--   ,"1r2r3/p3qppk/b1p2n1p/4Q3/1p2P2P/5N2/P2BNPP1/R3R1K1 b - - 0 1"
--   ,"1r2r3/p3qppk/b1p2n1p/4N3/1p2P2P/6Q1/P2BNPP1/R3R1K1 b - - 0 9"
  ]

test_sqs = [
  "E6","A7","D6","A5","E2","E8","B8","C8","F6","B2", "C7"
  ]
--   , "C6", "B5", "F8","C1","G6","C3","D1","G3","F7","B1","F4","B3","F5"
--   ,"G5"
--   , "F3", "E1","E3","H7","H4"
--   ]

spec_bools = [
  True, False, True, False, True, True, True, True, False, True, False
  ]
--   , False, False, False, True, True, True, True, False, False
--   , False, True, False, True, True, False, False, False, False, True
--   ]

games :: [Game]
games = map fen2Game fENs

sqs_test = map (fromJust . readSquare) test_sqs

out_bools = zipWith squareAttack sqs_test games

assertEq = assertEqual "falla: " :: Bool -> Bool -> Assertion

inputC = [ fst tc (snd tc) | tc <- zip inputA spec_bools]
  where inputA = map assertEq out_bools 

tests = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests
         if failures count > 0 then Exit.exitFailure else return ()
