module Search where

import Game ( Game, init_game, board, GamePos, gamePos )
import Moves ( Move, null_move )
import Data.Maybe ( fromJust, mapMaybe )
import Play ( makeMove )
import Valid ( genValidMoves )
import Evaluate ( evaluate, Score, _inf_score, inf_score, eval_factors )
import Generator (isCapture)
import Data.List (sortBy)
import qualified Data.Bifunctor

-- vars
max_depth = 22 :: Int

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
alphaBetaRoot = alphaBeta [] 0 _inf_score inf_score


isRep :: [GamePos] -> Game -> Bool
isRep gp g = gamePos g `elem` gp

alphaBeta :: [GamePos] -> Nodes -> Score -> Score -> Depth -> Game -> ScoreNodes
alphaBeta _  _  _ _ d _  | d < 0                = undefined
alphaBeta gp ni a b d g  | depthi >=  max_depth = (evaluate g,ni+1)
                         | isRep gp g           = (0,ni+1)
                         | d == 0               = qS gp ni a b g
                         | otherwise            = abIt (gpi:gp) ni a b
                           (d-1) sucPos
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        gpi = gamePos g
        depthi = length gp

abIt :: [GamePos] -> Nodes -> Score -> Score -> Depth -> [Game] -> ScoreNodes
abIt gp ni a _ _ []      = (a,ni)
abIt gp ni a b d (gi:gs) | si >= b    = (si,nf)
                         | si >  a    = abIt gp nf si b d gs
                         | otherwise  = abIt gp nf a b d gs

  where (si_,nf) = alphaBeta gp ni (negate b) (negate a) d gi
        si = negate si_

-- Quiescence
qS :: [GamePos] -> Nodes -> Score -> Score -> Game -> ScoreNodes
qS gp ni a b g | isRep gp g = (0,ni+1)
               | standPat >= b = (b,ni+1)
               | a < standPat = qS gp (ni+1) standPat b g
               | otherwise = qSIt (gpi:gp) ni a b sucGs
  where standPat = evaluate g
        moves = genValidMoves g
        captures = filter (isCapture bd) moves
        bd = board g
        sucGs = mapMaybe (`makeMove` g) captures
        gpi = gamePos g

qSIt :: [GamePos] -> Nodes -> Score -> Score -> [Game] -> ScoreNodes
qSIt gp ni a _ [] = (a,ni)
qSIt gp ni a b (gi:gs) | si >= b = (b,nf)
                       | si > a = qSIt gp nf si b gs
                       | otherwise = qSIt gp nf a b gs

  where (si_,nf) = qS gp ni (negate b) (negate a) gi
        si = negate si_

