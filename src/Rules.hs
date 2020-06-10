module ChessRules where

import Squares
import Pieces

tuple2Square :: (Int,Int) -> Square
tuple2Square (f,r) = Square f r

square2Tuple :: Square -> (Int,Int)
square2Tuple (Square f r) = (f,r)

chess_rules :: [(PieceType, Square -> [Square])]
chess_rules = [(Knight,knightMoves)]

knightMoves':: (Int,Int) -> [(Int,Int)]
knightMoves' (f,r) = [(f+1,r+2),(f+1,r-2),(f-1,r+2),(f-1,r-2),(f+2,r+1)
  ,(f+2,r-1),(f-2,r+1),(f-2,r-1)]

knightMoves :: Square -> [Square]
knightMoves sq = map tuple2Square (knightMoves' (square2Tuple sq))

onBoard' :: (Int,Int) -> Bool
onBoard' (f,r) | or [f<0,f>7,r<0,r>7] = False
               | otherwise = True  

onBoard :: Square -> Bool 
onBoard sq = onBoard' (square2Tuple sq) 

moveKnight' :: (Int,Int) -> [(Int,Int)]
moveKnight' tp | onBoard' tp = filter onBoard' (knightMoves' tp)
              | otherwise  = [] 

moveKnight :: Square -> [Square]
moveKnight sq | onBoard sq = filter onBoard (knightMoves sq)
              | otherwise  = [] 
