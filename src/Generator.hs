module Generator where
import Game ( Game, GameState(board) )
import Pieces ( Piece )
import Rules ( isFriendly, onBoard', piece_map, CBranch, Dir(..) )
import Squares ( Square, square2Tuple, tuple2Square )
import Board ( checkSquare )
import Data.Maybe ( fromMaybe )
import Moves ( Move(Move) )


squareAttack :: Square -> Game -> Bool
squareAttack _ _ = False


moveGenerator :: Game -> [Move]
moveGenerator _ = []


makeSquares' :: (Int,Int) -> [[Dir]] -> [(Int,Int)]
makeSquares' sq = map $ compose sq


makeSquares :: Square -> [[Dir]] -> [Square]
makeSquares sq ds = map tuple2Square (makeSquares' (square2Tuple sq) ds)


addTuple' :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuple' (f1,r1) (f2,r2) = (f1+f2,r1+r2)


compose :: (Int,Int) -> [Dir] -> (Int,Int)
compose inp ds = addTuple' (foldr orthoMove (0,0) ds) inp


orthoMove :: Dir -> (Int,Int) -> (Int,Int)
orthoMove d (f,r) | d == North = (f,r+1)
             | d == South = (f,r-1)
             | d == East  = (f+1,r)
             | d == West  = (f-1,r)

branchPiece :: Piece -> CBranch
branchPiece p = fromMaybe [] (lookup p piece_map)


squaresForSide:: Game -> [Square]
squaresForSide g = []

makeMove :: Square -> Square -> Move
makeMove is fs = Move is fs Nothing

squareMoveGen :: Square -> Game -> [Move]
squareMoveGen s g = map (makeMove s) (filter (isFriendly s
  (board g)) (map tuple2Square (filter onBoard' (makeSquares'
  (square2Tuple s) (maybe [] branchPiece (checkSquare s
  (board g)))))))
