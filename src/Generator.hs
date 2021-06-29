module Generator where
import Game
import Pieces
import Rules
import Squares
import Board
import Data.Maybe ( Maybe(Just, Nothing) )
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moves

squareAttack :: Square -> Game -> Bool
squareAttack _ _ = False

moveGenerator :: Game -> [Move]
moveGenerator _ = []


makeSquares' :: (Int,Int) -> [CPath] -> [(Int,Int)]
makeSquares' sq = map $ compose sq


makeSquares :: Square -> [CPath] -> [Square]
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

-- 
-- 

genMoveFromBranches :: Square -> Board -> CBranch -> [Move]
genMoveFromBranches s b c = map (makeMove s) (filter (isAStep s
  b) (map tuple2Square (filter onBoard' (makeSquares'
  (square2Tuple s) c))))

mkRaysFromBranches :: Square -> Board -> CBranch -> [[Square]]
mkRaysFromBranches sq bd = map (mkRay sq bd)

mkRay :: Square -> Board -> CPath -> [Square]
mkRay sq bd di = map tuple2Square (mkRayIter sq bd di 7 [])

mkRayIter :: Square -> Board -> CPath -> Int -> [(Int,Int)] -> [(Int,Int)]
mkRayIter _ _ _ 0 isq = isq
mkRayIter sq bd dr n [] = mkRayIter sq bd dr (n - 1)
  (makeSquares' (square2Tuple sq) [dr])
mkRayIter sq bd dr n isq | onBoard' (head isq) && isEmpty (tuple2Square $
  head isq) bd = mkRayIter sq bd dr (n - 1) (makeSquares' (head isq) [dr])
                 ++ isq
                         | onBoard' (head isq) && not (sameSideStep sq bd
  (head (map tuple2Square isq))) = isq
                         | otherwise = tail isq

makeMove :: Square -> Square -> Move
makeMove is fs = Move is fs Nothing

makeCrownPawnMove :: Square -> Square -> PieceType -> Move
makeCrownPawnMove is fs pt = Move is fs (Just pt)

genKnightMoves :: Square -> Board -> [Move]
genKnightMoves s b = genMoveFromBranches s b knight_branches 


genRookMoves :: Square -> Board -> [Move]
genRookMoves s b = map (makeMove s) $ concat $ mkRaysFromBranches s b
  mini_rook_branches

genBishopMoves :: Square -> Board -> [Move]
genBishopMoves s b = map (makeMove s) $ concat $ mkRaysFromBranches s b
  mini_bishop_branches 

genQueenMoves :: Square -> Board -> [Move]
genQueenMoves sq bd = genRookMoves sq bd ++ genBishopMoves sq bd
