module Search where

import Game ( Game, init_game, board )
import Moves ( Move, null_move )
import Data.Maybe ( fromJust, mapMaybe )
import Play ( makeMove )
import Valid ( genValidMoves )
import Evaluate ( evaluate, Score, _inf_score, inf_score )
import Generator (isCapture)
import Data.List (sortBy)
import qualified Data.Bifunctor

-- types
type Depth = Int
type Nodes = Int
type MoveScore = (Move,Score)
type SearchInfo = (Move,Score,Nodes)
type ScoreNodes = (Score,Nodes)

-- funcs
searchDivide :: Game -> Depth -> [SearchInfo]
searchDivide g d | d < 0 = []
                 | d == 0 = [(null_move,evaluate g,1)]
                 | otherwise = sortBy (\(_,a,_) (_,b,_) -> compare a b)
                                 mScores
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        scoresNodes = map (Data.Bifunctor.first negate . search (d-1)) sucPos
        mScores = zipWith (\a (b,c) -> (a,b,c)) moves scoresNodes


search :: Depth -> Game -> ScoreNodes
-- search = negaMax
search = alphaBetaRoot


-- negaMax :: Depth -> Game  -> Score
-- negaMax d g | d < 0 = undefined
--             | d == 0 = evaluate g
--             | otherwise = maximum (map (negate . negaMax (d-1))
--                                    sucPos)
--   where moves = genValidMoves g
--         sucPos = mapMaybe (`makeMove` g) moves


alphaBetaRoot :: Depth -> Game -> ScoreNodes
alphaBetaRoot = alphaBeta 0 _inf_score inf_score


alphaBeta :: Nodes -> Score -> Score -> Depth -> Game -> ScoreNodes
alphaBeta _  _ _ d _  | d < 0    = undefined
alphaBeta ni a b d g  | d == 0   = qS ni a b g
alphaBeta ni a b d g  = abIt ni a b (d-1) sucPos
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves

abIt :: Nodes -> Score -> Score -> Depth -> [Game] -> ScoreNodes
abIt ni a _ _ []      = (a,ni)
abIt ni a b d (gi:gs) | si >= b    = (si,nf)
                      | si >  a    = abIt nf si b d gs
                      | otherwise  = abIt nf a b d gs

  where (si_,nf) = alphaBeta ni (negate b) (negate a) d gi
        si = negate si_

-- Quiescence
qS :: Nodes -> Score -> Score -> Game -> ScoreNodes
qS ni a b g | standPat >= b = (b,ni+1)
            | a < standPat = qS (ni+1) standPat b g
            | otherwise = qSIt ni a b sucGs
  where standPat = evaluate g
        moves = genValidMoves g
        captures = filter (isCapture bd) moves
        bd = board g
        sucGs = mapMaybe (`makeMove` g) captures

qSIt :: Nodes -> Score -> Score -> [Game] -> ScoreNodes
qSIt ni a _ [] = (a,ni)
qSIt ni a b (gi:gs) | si >= b = (b,nf)
                    | si > a = qSIt nf si b gs
                    | otherwise = qSIt nf a b gs

  where (si_,nf) = qS ni (negate b) (negate a) gi
        si = negate si_
