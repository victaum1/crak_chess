module Rules where

import Data.Maybe
import Squares
import Pieces

data Dir = North | South | East | West deriving (Eq,Show)

tuple2Square :: (Int,Int) -> Square
tuple2Square (f,r) = Square f r

square2Tuple :: Square -> (Int,Int)
square2Tuple (Square f r) = (f,r)

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

makeSquares' :: (Int,Int) -> [[Dir]] -> [(Int,Int)]
makeSquares' sq ds = compose <$> ds <*> [sq]

makeSquares :: Square -> [[Dir]] -> [Square]
makeSquares sq ds = map tuple2Square (makeSquares' (square2Tuple sq) ds) 

mini_rook_rules = [[North],[South],[East],[West]]

mini_bishop_rules = [[North,East],[South,East],[South,West],[North,West]]

king_rules = mini_rook_rules ++ mini_bishop_rules

addTuple' :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuple' (f1,r1) (f2,r2) = (f1+f2,r1+r2)

compose :: [Dir] -> (Int,Int) -> (Int,Int)
compose ds = addTuple' (foldr orthoMove (0,0) ds) 

orthoMove :: Dir -> (Int,Int) -> (Int,Int)
orthoMove d (f,r) | d == North = (f,r+1)
             | d == South = (f,r-1)
             | d == East  = (f+1,r)
             | d == West  = (f-1,r)

onEmptyBoard' :: (Int,Int) -> Bool
onEmptyBoard' (f,r) | or [f<0,f>7,r<0,r>7] = False
                    | otherwise = True  

onEmptyBoard :: Square -> Bool
onEmptyBoard sq = onEmptyBoard' (square2Tuple sq)

seekMap :: (Eq a) => a -> [(a,c)] -> c
seekMap t m = fromJust (lookup t m)

movePieceType :: PieceType -> Square -> [Square]
movePieceType pt sq = filter onEmptyBoard (makeSquares sq ruleMap)
  where ruleMap = seekMap pt map_pieces

movePawn :: Piece -> Square -> [Square]
movePawn pn sq = filter onEmptyBoard (makeSquares sq ruleMap)
  where ruleMap = seekMap pn map_pawns 

movePiece' :: Piece -> Square -> [Square]
movePiece' p sq | pieceType p == Pawn = movePawn p sq
                | otherwise = movePieceType (pieceType p) sq

movePiece :: Piece -> Square -> [Square]
movePiece p sq | onEmptyBoard sq = movePiece' p sq
               | otherwise = []

rook_rules = replicate <$> [1..7] <*> (concat mini_rook_rules)

bishop_rules = map concat (replicate <$> [1..7] <*> mini_bishop_rules)

queen_rules = rook_rules ++ bishop_rules

