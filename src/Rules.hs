module Rules where
import Data.Maybe ( fromMaybe, isNothing )
import Pieces
    ( PieceType(Knight), Piece(Piece, pieceSide), Side(Black, White) )
import Board ( Board, checkSquare)
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Squares ( Square, square2Tuple )

-- data MoveLike = Slider | Jumper | OneStep deriving (Eq,Show)
data Dir = North | South | East | West deriving (Eq,Show)
type CPath = [Dir]
type CBranch = [[Dir]]

knight_branches = [[North,North,East],[North,North,West]
                  ,[South,South,East],[South,South,West]
                  ,[East,East,North],[East,East,South]
                  ,[West,West,North],[West,West,South]]

white_pawn_normal_b = [[North]]
white_pawn_double_step =[[North,North]] 
black_pawn_normal_b = [[South]]
black_pawn_double_step = [[South,South]]
mini_rook_branches = [[North],[South],[East],[West]]
mini_bishop_branches = [[North,East],[South,East],[South,West]
  ,[North,West]]
mini_queen_branches = mini_rook_branches ++ mini_bishop_branches
king_normal = mini_queen_branches


-- Predicates for branches
onBoard' :: (Int,Int) -> Bool
onBoard' (f,r) | or [f<0,f>7,r<0,r>7] = False
               | otherwise = True

onBoard :: Square -> Bool
onBoard sq = onBoard' (square2Tuple sq)


sameSide :: Piece -> Piece -> Bool
sameSide p1 p2 = pieceSide p1 == pieceSide p2 

isEmpty :: Square -> Board -> Bool
isEmpty sq bd = isNothing (checkSquare sq bd)

isFriendly :: Square -> Board -> Square ->  Bool
isFriendly isq bd fsq = isEmpty fsq bd && not (fromMaybe False (sameSide <$> checkSquare isq bd <*> checkSquare fsq bd))
