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


alphaBeta :: Score  -> Score -> Score -> Depth -> Game -> Score
alphaBeta a b r d g | d <= 0 = evalPos g
                    | otherwise = score
  where  pickScore si r = if si > r then si else r
         pickAlpha si a = if si > a then si else a
         moves = genValidMoves g
         sucPos = mapMaybe (`makeMove` g) moves
         scoreIt ai bi si di = negate . alphaBeta (negate bi) (negate ai)
                 si (di-1)
         rsu = [pickScore si r | gi <- sucPos
                 , let si = scoreIt (pickAlpha r a) b r d gi
                 , si < b]
         rs = sort rsu
         score = last rs


search :: Depth -> Game -> MoveScore
search d g | d <= 0 = (null_move,evalPos g)
           | otherwise = (move,score)
 where moves = genValidMoves g
       sucPos = mapMaybe (`makeMove` g) moves
       rsu = map (negate . alphaBeta (negate inf_score) inf_score
                  (negate inf_score) (d - 1)) sucPos
       mScore = zip moves rsu
       mso = sortOn snd mScore
       mtp = last mso
       move = fst mtp
       score = negate (snd mtp)
