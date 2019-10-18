module Main (main) where
import Test.HUnit
import Game
import qualified System.Exit as Exit

inputFENs = [
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
  "r2qkbnr/pppb1ppp/2Np4/1B6/4P3/8/PPP2PPP/RNBQK2R b KQkq - 0 6",
  "r2qkbnr/ppp2ppp/2bp4/1B6/4P3/8/PPP2PPP/RNBQK2R w KQkq - 0 7",
  "r2qkbnr/ppp2ppp/2Bp4/8/4P3/8/PPP2PPP/RNBQK2R b KQkq - 0 7",
  "r2qkbnr/p1p2ppp/2pp4/8/4P3/8/PPP2PPP/RNBQK2R w KQkq - 0 8",
  "r2qkbnr/p1p2ppp/2pp4/8/4P3/8/PPP2PPP/RNBQ1RK1 b kq - 1 8",
  "r2qkb1r/p1p2ppp/2pp1n2/8/4P3/8/PPP2PPP/RNBQ1RK1 w kq - 2 9"]

assertEq = assertEqual "Falla: " :: String -> String -> Assertion

inputs = map fen2Game inputFENs

inputC = [ fst tc (snd tc) | tc <- zip inputA inputB]
  where inputA = map assertEq inputFENs
        inputB = map game2FEN inputs

tests = TestList $ map TestCase inputC

main :: IO ()
main = do
         count <- runTestTT tests
         if failures count > 0 then Exit.exitFailure else return ()
