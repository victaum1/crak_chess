module Rules where
import Data.Maybe ( fromMaybe, isNothing )
import Pieces
    ( PieceType(Knight), Piece(Piece, pieceSide), Side(Black, White) )
import Board ( checkSquare, Board )
import qualified Data.Set as Set
import Squares ( Square, square2Tuple )


data MoveLike = Slider | Jumper | KinG | PawN deriving (Eq,Show)
data Dir = North | South | East | West deriving (Eq,Show)
type CPath = [Dir]
type CBranch = [[Dir]]


knight_branches = [[North,North,East],[North,North,West],[South,South,East]
  ,[South,South,West],[East,East,North],[East,East,South],[West,West,North]
  ,[West,West,South]]

white_pawn_branches = [[North],[North,North],[North,East],[North,West]]
black_pawn_branches = [[South],[South,South],[South,East],[South,West]]


mini_rook_branches = [[North],[South],[East],[West]]
mini_bishop_branches = [[North,East],[South,East],[South,West],[North,West]]
king_branches = [[East,East],[West,West]] ++ mini_queen_branches
mini_queen_branches = mini_rook_branches ++ mini_bishop_branches


-- Predicates for branches
onBoard' :: (Int,Int) -> Bool
onBoard' (f,r) | or [f<0,f>7,r<0,r>7] = False
                    | otherwise = True

onBoard :: Square -> Bool
onBoard sq = onBoard' (square2Tuple sq)


sameSide :: Piece -> Piece -> Bool
sameSide p1 p2 = pieceSide p1 == pieceSide p2 

isEmpty :: Square -> Board -> Bool
isEmpty sq bd = isNothing (lookup sq (Set.toList bd))

isFriendly :: Square -> Board -> Square -> Bool
isFriendly isq bd fsq = isEmpty fsq bd && not (fromMaybe False (sameSide <$> checkSquare isq bd <*> checkSquare fsq bd))

piece_map = [
  (Piece White Knight,knight_branches)
  ,(Piece Black Knight,knight_branches)
--  ,(Piece White Rook,mini_rook_branches)
--  ,(Piece Black Rook,mini_rook_branches)
--  ,(Piece White Bishop,mini_bishop_branches)
--  ,(Piece Black Bishop,mini_bishop_branches)
--  ,(Piece White Queen,mini_queen_branches)
--  ,(Piece Black Queen,mini_queen_branches)
--  ,(Piece White Pawn, white_pawn_branches)
--  ,(Piece Black Pawn, black_pawn_branches)
--  ,(Piece White King, king_branches)
--  ,(Piece Black King, king_branches)
  ]
