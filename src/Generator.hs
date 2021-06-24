module Generator where
import Game ( Game, GameState(board) )
import Pieces ( Piece, PieceType )
import Rules ( knight_branches, isAStep, onBoard', onBoard, CBranch,
               Dir(..), mini_rook_branches )
import Squares ( Square, square2Tuple, tuple2Square )
import Board (checkSquare, Board(..))
import Data.Maybe ( fromMaybe )
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moves (Move(..))


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


squaresForSide :: Game -> [Square]
squaresForSide g = []


makeMove :: Square -> Square -> Move
makeMove is fs = Move is fs Nothing

makeCrownPawnMove :: Square -> Square -> PieceType -> Move
makeCrownPawnMove is fs pt = Move is fs (Just pt)

genKnightMoves :: Square -> Board -> [Move]
genKnightMoves s b = genMoveFromBranches s b knight_branches 

genMoveFromBranches :: Square -> Board -> CBranch -> [Move]
genMoveFromBranches s b c = map (makeMove s) (filter (isAStep s
  b) (map tuple2Square (filter onBoard' (makeSquares'
  (square2Tuple s) c))))

genRookMoves :: Square -> Board -> [Move]
genRookMoves s b = map (makeMove s) $ concat $ mkRaysFromBranches s b mini_rook_branches

mkRaysFromBranches :: Square -> Board -> CBranch -> [[Square]]
mkRaysFromBranches sq bd cb = map (mkRay sq bd) (concat cb)

mkRay :: Square -> Board -> Dir -> [Square]
mkRay sq bd di = map tuple2Square (mkRayIter sq bd di 7 [])

mkRayIter :: Square -> Board -> Dir -> Int -> [(Int,Int)] -> [(Int,Int)]
mkRayIter _ _ _ 0 isq = isq
mkRayIter sq bd dr n [] = mkRayIter sq bd dr (n - 1)
  (makeSquares' (square2Tuple sq) [[dr]])
mkRayIter sq bd dr n isq | onBoard' (head isq) && isAStep sq bd
  (head (map tuple2Square isq)) = mkRayIter sq bd dr (n - 1)
                               (makeSquares' (head isq) [[dr]]) ++ isq
                         | otherwise = tail isq
