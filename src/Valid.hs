module Valid where
import Generator
import Play
import Board
import Game
import Moves
import Pieces(Side)
import Data.Maybe

isMoveValid :: Game -> Move -> Bool
isMoveValid g m = maybe False (not . isBoardInCheck s) nb
 where ng = makeMove m g
       nb = board <$> ng
       s = turn g

genValidMoves :: Game -> [Move]
genValidMoves g = filter (isMoveValid g) $ moveGenerator g
  where b = board g
        s = turn g
