module Search where

import Game ( Game, init_game )
import Moves ( Move, null_move )
import Data.Maybe ( fromJust, mapMaybe )
import Play ( makeMove )
import Valid ( genValidMoves )
import Evaluate ( evaluate, Score )

-- types
type Depth = Int
type MoveScore = (Move,Score)

-- funcs
searchDivide :: Game -> Depth -> [MoveScore]

searchDivide g d | d < 0 = []
                 | d == 0 = [(null_move,evaluate g)]
                 | otherwise = zip moves scores
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        scores = map (`search` (d-1)) sucPos


search :: Game -> Depth -> Score
search = negaMax

negaMax :: Game -> Depth -> Score

negaMax g d | d < 0 = undefined
            | d == 0 = evaluate g
            | otherwise = maximum (map ((* (-1)) . (`negaMax` (d-1)))
                                   sucPos)
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
