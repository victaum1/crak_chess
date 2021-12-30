module Search where

import Game ( Game, init_game, board )
import Moves ( Move, null_move )
import Data.Maybe ( fromJust, mapMaybe )
import Play ( makeMove )
import Valid ( genValidMoves )
import Evaluate ( evaluate, Score, _inf_score, inf_score )
import Generator (isCapture)
import Data.List (sortBy)

-- types
type Depth = Int
type Nodes = Int
type MoveScore = (Move,Score)
type SearchInfo = (Move,Score,Nodes)

-- funcs
searchDivide :: Game -> Depth -> [MoveScore]
searchDivide g d | d < 0 = []
                 | d == 0 = [(null_move,evaluate g)]
                 | otherwise = sortBy (\(_,a) (_,b) -> compare a b)
                                 mScores
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        scores = map ((* (-1)) . search (d-1)) sucPos
        mScores = zip moves scores


search :: Depth -> Game -> Score
-- search = negaMax
search = alphaBetaRoot


negaMax :: Depth -> Game  -> Score
negaMax d g | d < 0 = undefined
            | d == 0 = evaluate g
            | otherwise = maximum (map (negate . negaMax (d-1))
                                   sucPos)
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves


alphaBetaRoot :: Depth -> Game -> Score
alphaBetaRoot = alphaBeta _inf_score inf_score


alphaBeta :: Score -> Score -> Depth -> Game -> Score
alphaBeta _ _ d _ | d < 0    = undefined
alphaBeta a b d g | d == 0   = qS a b g
alphaBeta a b d g  = abIt a b (d-1) sucPos
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves

abIt :: Score -> Score -> Depth -> [Game] -> Score
abIt a _ _ []      = a
abIt a b d (gi:gs) | si >= b = si
                     | si > a = abIt si b d gs
                     | otherwise  = abIt a b d gs

  where si = negate $ alphaBeta (negate b) (negate a) d gi


-- Quiescence
qS :: Score -> Score -> Game -> Score
qS a b g | standPat >= b = b
              | a < standPat = qS standPat b g
              | otherwise = qSIt a b sucGs
  where standPat = evaluate g
        moves = genValidMoves g
        captures = filter (isCapture bd) moves
        bd = board g
        sucGs = mapMaybe (`makeMove` g) captures

qSIt :: Score -> Score -> [Game] -> Score
qSIt a _ [] = a
qSIt a b (gi:gs) | si >= b = b
                    | si > a = qSIt si b gs
                    | otherwise = qSIt a b gs

  where si = negate $ qS (negate b) (negate a) gi
