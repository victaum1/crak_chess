module Search where

-- import Data.Maybe
-- import Data.Bifunctor
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
import Moves ( Move(Move), null_move, isNullMove)
import Evaluate ( evalPos, mate_score, Score )
import Game ( Game )
import Squares ( Square(Square) )
import Valid ( genValidMoves )
import Play ( makeMove )

type Depth = Int

type Nodes = Int

type ABInfo = (Score,Nodes)

type MoveScore = (Move,Score)

type MoveInfo = (Move,Score,Nodes)

inf_score = mate_score + 1
_inf_score = negate inf_score


alphaBeta :: Nodes -> Score -> Score -> Score -> Depth -> Game -> ABInfo
alphaBeta n a b r d g | d <= 0 = (evalPos g,n+1)
                      | otherwise = if score <= negate mate_score
                          then (negate mate_score + d, tn) else (score, tn)
  where moves = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) moves
        bsn = betaCut n a b r d sucPos
        score = fst bsn
        tn = snd bsn

betaCut :: Nodes -> Score -> Score -> Score -> Depth -> [Game] -> ABInfo
betaCut n a b r d [] = (r,n)
betaCut n a b r d (gi:gs) = bnext
  where (si,n_) = alphaBeta 0 (negate b) (negate a) r (d-1) gi
        nsi = negate si
        anext = if nsi > a then nsi else a
        bnext = if nsi >= b then (rnext,nnext) else betaCut nnext anext b rnext (d-1) gs
        rnext = if nsi > r then nsi else r
        nnext = n + n_


searchList :: Depth -> Game -> [MoveInfo]
searchList d g | d <= 0 = [(null_move,evalPos g,1)]
               | otherwise = sortBy (\(_,a,_) (_,b,_) -> compare a b) mns
  where ms = genValidMoves g
        sucPos = mapMaybe (`makeMove` g) ms
        npb = map (alphaBeta 0 _inf_score inf_score _inf_score (d - 1))
          sucPos
        msn = zipWith (\a (s,n) -> (a,s,n)) ms npb
        mns = map (\(a,s,n)->(a,negate s,n)) msn

search :: Depth -> Game -> MoveInfo
search d g = (move,score,tn)
 where mso = searchList d g
       nullMso = null mso
       msi = if nullMso then (null_move,_inf_score,0) else last mso
       (move,_,_) = msi
       (_,score,_) = msi
       trd (a,b,c) = c
       tn = sum $ map trd mso
