module Perft where

-- import Game
-- import Valid
-- import Play
-- import Data.Maybe


import Game ( Game, pGame )
import Valid ( genValidMoves )
import Play ( makeMove )
import Data.Maybe ( mapMaybe, fromJust, isNothing )

perft :: Game -> Int -> Int
perft p d | d == 0 = 1
          | otherwise = sum $ map (`perft` (d-1)) sucPos
  where moves = genValidMoves p
        sucPos = mapMaybe (`makeMove` p) moves

