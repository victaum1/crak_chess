module Generator (moveGenerator) where
import Squares
import Pieces
import Game
import Moves
import Rules


squareAttack :: Square -> Game -> Bool
squareAttack _ _ = False


moveGenerator :: Game -> [Move]
moveGenerator _ = undefined


makeSquares' :: (Int,Int) -> [[Dir]] -> [(Int,Int)]
makeSquares' sq ds = compose <$> ds <*> [sq]


makeSquares :: Square -> [[Dir]] -> [Square]
makeSquares sq ds = map tuple2Square (makeSquares' (square2Tuple sq) ds)


addTuple' :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuple' (f1,r1) (f2,r2) = (f1+f2,r1+r2)

compose :: [Dir] -> (Int,Int) -> (Int,Int)
compose ds = addTuple' (foldr orthoMove (0,0) ds) 

orthoMove :: Dir -> (Int,Int) -> (Int,Int)
orthoMove d (f,r) | d == North = (f,r+1)
             | d == South = (f,r-1)
             | d == East  = (f+1,r)
             | d == West  = (f-1,r)


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

