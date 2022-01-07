module Perft where

import Data.Maybe ( mapMaybe, fromJust )
import Moves ( Move )
import Game ( Game, init_game)
import Valid ( genValidMoves )
import Play ( makeMove)

perft :: Game -> Int -> Int
perft p d | d == 0 = 1
          | otherwise = sum $ map (`perft` (d-1)) sucPos
  where moves = genValidMoves p
        sucPos = mapMaybe (`makeMove` p) moves

