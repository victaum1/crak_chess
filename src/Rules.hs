module Rules where

import Data.Maybe
import Squares
import Pieces

data Dir = North | South | East | West deriving (Eq,Show)

map_pieces :: [(PieceType, [[Dir]])]
map_pieces = [
    (Knight, knight_rules)
  , (Bishop, bishop_rules) 
  , (Rook,   rook_rules) 
  , (Queen,  queen_rules) 
  , (King,   king_rules) 
  ]

map_pawns :: [(Piece, [[Dir]])]
map_pawns = [
    (Piece White Pawn, white_pawn_rules)
  , (Piece Black Pawn, black_pawn_rules)
  ]

knight_rules = [[North,North,East],[North,North,West],[South,South,East]
  ,[South,South,West],[East,East,North],[East,East,South],[West,West,North]
  ,[West,West,South]]

white_pawn_rules = [[North],[North,North],[North,East],[North,West]]
black_pawn_rules = [[South],[South,South],[South,East],[South,West]]


mini_rook_rules = [[North],[South],[East],[West]]

mini_bishop_rules = [[North,East],[South,East],[South,West],[North,West]]

king_rules = mini_rook_rules ++ mini_bishop_rules



onEmptyBoard' :: (Int,Int) -> Bool
onEmptyBoard' (f,r) | or [f<0,f>7,r<0,r>7] = False
                    | otherwise = True  

onEmptyBoard :: Square -> Bool
onEmptyBoard sq = onEmptyBoard' (square2Tuple sq)

seekMap :: (Eq a) => a -> [(a,c)] -> c
seekMap t m = fromJust (lookup t m)


rook_rules = replicate <$> [1..7] <*> (concat mini_rook_rules)

bishop_rules = map concat (replicate <$> [1..7] <*> mini_bishop_rules)

queen_rules = rook_rules ++ bishop_rules

