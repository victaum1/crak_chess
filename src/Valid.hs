module Valid where
import Generator
import Board
import Game
import Moves
import Pieces(Side)
import Data.Maybe

isMoveValid :: Board -> Side -> Move -> Bool
isMoveValid b s m = maybe False (not . isBoardInCheck s) nb
 where nb = mkMoveBoard m b

genValidMoves :: Game -> [Move]
genValidMoves g = filter (isMoveValid b s) $ moveGenerator g
  where b = board g
        s = turn g
