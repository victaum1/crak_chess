module Perft where

import Data.Maybe
import Game
import Valid
import Play

perft :: Game -> Int -> Int
perft p d | d == 0 = 1
          | otherwise = sum $ map (`perft` (d-1)) sucPos
  where moves = genValidMoves p
        sucPos = mapMaybe (`makeMove` p) moves

