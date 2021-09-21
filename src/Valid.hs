module Valid where


import Generator
import Play
import Board
import Game
import Moves
import Pieces
import Data.Maybe


isMate :: Game -> Bool
isMate g | isInCheck g && null (genValidMoves g) = True
         | otherwise = False


isStaleMate g | not (isInCheck g) && null (moveGenerator g)
  = True
              | otherwise = False


isDrawBy50 g | np >= 100 = True
             | otherwise = False
  where np = nPlys g


isDraw g = isStaleMate g || isDrawBy50 g


isTerminal g = isMate g || isDraw g


isMoveValid :: Game -> Move -> Bool
isMoveValid g m = maybe False
  (not . isBoardInCheck s) nb
  where ng = makeMove m g
        nb = board <$> ng
        s  = turn g


genValidMoves :: Game -> [Move]
genValidMoves g = filter (isMoveValid g) $ moveGenerator g
 
