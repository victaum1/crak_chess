module Search where

-- import Data.Maybe
-- import Data.List
-- import Moves
-- import Evaluate
-- import Game
-- import Squares
-- import Valid
-- import Play

import Data.Maybe ( mapMaybe )
import Data.List ( sortOn, sortBy, sort )
import Data.Bifunctor (first)
import Moves ( Move(Move) )
import Evaluate ( evalPos, mate_score, Score )
import Game ( Game )
import Squares ( Square(Square) )
import Valid ( genValidMoves )
import Play ( makeMove )

type Depth = Int

type Nodes = Int

type ABInfo = (Score,Nodes)

type MoveInfo = (Move,Score,Nodes)

inf_score = mate_score + 1
_inf_score = negate inf_score

null_move = Move (Square 0 0) (Square 0 0) Nothing


alphaBeta :: Nodes -> Score -> Score -> Score -> Depth -> Game -> ABInfo
alphaBeta n a b r d g | d <= 0 = (evalPos g,n+1)
                      | otherwise = if nscore <= mate_score then
                          (nscore+d, tn) else (nscore, tn)
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        rsu = betaCut n (negate b) (negate a) r (d-1) sucPos
        rsu' = map (first negate) rsu
        rso = sortOn fst rsu'
        mtp = if null rso then (_inf_score,0) else last rso
        score = fst mtp
        nscore = negate score
        tn = if null rso then 0 else sum $ map snd rso


betaCut :: Nodes -> Score -> Score -> Score -> Depth -> [Game] -> [ABInfo]
betaCut n a b r d [] = []
betaCut n a b r d (gi:gs) | si > r = (si,n_) : if si < b then [] else
                              betaCut n a b r d gs
                          | otherwise = (r,n_) : betaCut n a b r d gs
  where (si,n_) = alphaBeta n (pickAlpha r a) b r d gi
        pickAlpha ri ai = if ri > ai then ri else ai


search :: Depth -> Game -> MoveInfo
search d g | d <= 0 = (null_move,evalPos g, 1)
           | otherwise = if nscore <= mate_score then (move,nscore+d,tn)
                         else (move,nscore,tn)
 where moves = genValidMoves g
       sucPos = mapMaybe (`makeMove` g) moves
       rsu = map (alphaBeta 0 (negate inf_score) inf_score
                  (negate inf_score) (d - 1)) sucPos
       rsu' = map (first negate) rsu
       msn = zipWith (\m (s,n) -> (m,s,n))  moves rsu'
       mso = sortBy (\(_,a,_) (_,b,_)-> compare a b) msn
       mtp = if null mso then (null_move,_inf_score,0) else last mso
       (move,_,_) = mtp
       (_,score,_) = mtp
       nscore = negate score
       tkl (a,b,c) = c
       tn = if null mso then 0 else sum $ map tkl mso
