module Search where

import Data.Maybe
import Data.List
import Moves
import Evaluate
import Game
import Squares
import Valid
import Play

type Depth = Int

type MoveScore = (Move,Score)

inf_score = max_material_score + 100

null_move = Move (Square 0 0) (Square 0 0) Nothing


negaMaxIter :: Score -> Depth -> Game -> Score
negaMaxIter r d g | d == 0 = evalPos g
                  | otherwise = if score > r then score else r
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        rsu = map (negaMaxIter r (d - 1)) sucPos
        rs = sort rsu
        mScore = head rs
        score = negate mScore


search :: Depth -> Game -> MoveScore
search d g | d <= 0 = (null_move,evalPos g)
           | otherwise = (move,score)
 where moves = genValidMoves g
       sucPos = mapMaybe (`makeMove` g) moves
       rsu = map (negaMaxIter (negate inf_score) (d - 1)) sucPos
       mScore = zip moves rsu
       mso = sortOn snd mScore
       mtp = head mso
       move = fst mtp
       score = negate (snd mtp)

