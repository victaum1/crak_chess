module Generator where
import Game
import Pieces
import Rules
import Squares
import Board
import Data.Maybe (Maybe(Just, Nothing), mapMaybe)
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moves


squareAttack :: Square -> Game -> Bool
squareAttack sq ge = squareAttackOnBoard inv sq aBoard
  where aSide = turn ge
        aBoard = board ge
        inv = if aSide == White then Black else White

squareAttackOnBoard :: Side -> Square -> Board -> Bool
squareAttackOnBoard si sq bd = squareAttackByKnight sq si bd ||
  squareAttackByBishop sq si bd || squareAttackByRook sq si bd ||
  squareAttackByQueen sq si bd  || squareAttackByPawn sq si bd
  
squareAttackByKnight :: Square -> Side -> Board -> Bool
squareAttackByKnight sq si bd =  any (isSideKnight si)
  (mapMaybe (`checkSquare` bd) (genKnightSquares sq Map.empty))

isSideKnight :: Side -> Piece -> Bool
isSideKnight s p = Piece s Knight == p

squareAttackByQueen :: Square -> Side -> Board -> Bool
squareAttackByQueen sq si bd =  any (isSideQueen si)
  (mapMaybe (`checkSquare` bd) (genQueenSquares sq Map.empty))

isSideQueen :: Side -> Piece -> Bool
isSideQueen s p = Piece s Queen == p

squareAttackByBishop :: Square -> Side -> Board -> Bool
squareAttackByBishop sq si bd =  any (isSideBishop si)
  (mapMaybe (`checkSquare` bd) (genBishopSquares sq Map.empty))

isSideBishop :: Side -> Piece -> Bool
isSideBishop s p = Piece s Bishop == p

squareAttackByRook :: Square -> Side -> Board -> Bool
squareAttackByRook  sq si bd =  any (isSideRook si)
  (mapMaybe (`checkSquare` bd) (genRookSquares sq Map.empty))

isSideRook :: Side -> Piece -> Bool
isSideRook s p = Piece s Rook == p

squareAttackByPawn :: Square -> Side -> Board -> Bool
squareAttackByPawn sq si bd = any (isSidePawn si)
  (mapMaybe (`checkSquare` bd) (genSidePawnSquares inv sq Map.empty))
  where inv | si == White = Black
            | otherwise = White

isSidePawn :: Side -> Piece -> Bool
isSidePawn s p = Piece s Pawn == p

genSidePawnSquares :: Side -> Square -> Board -> [Square]
genSidePawnSquares si sq bd | si == White = genSquaresFromBranches sq bd
  white_pawn_captures 
                            | otherwise = genSquaresFromBranches sq bd
  black_pawn_captures


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

genMoveFromBranches :: Square -> Board -> CBranch -> [Move]
genMoveFromBranches s b c = map (makeSimpleMove s)  $
  genSquaresFromBranches s b c

genSquaresFromBranches :: Square -> Board -> CBranch -> [Square]
genSquaresFromBranches s b c = filter (isAStep s
  b) (map tuple2Square (filter onBoard' (makeSquares'
  (square2Tuple s) c)))


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

makeSimpleMove :: Square -> Square -> Move
makeSimpleMove is fs = Move is fs Nothing

makeCrownMove :: Square -> Square -> PieceType -> Move
makeCrownMove is fs pt = Move is fs (Just pt)

genKnightMoves :: Square -> Board -> [Move]
genKnightMoves s b = genMoveFromBranches s b knight_branches

genKnightSquares :: Square -> Board -> [Square]
genKnightSquares s b = genSquaresFromBranches s b knight_branches

genRookMoves :: Square -> Board -> [Move]
genRookMoves s b = map (makeSimpleMove s) $ genRookSquares s b

genRookSquares :: Square -> Board -> [Square]
genRookSquares s b = concat $ mkRaysFromBranches s b
  mini_rook_branches

genBishopMoves :: Square -> Board -> [Move]
genBishopMoves s b = map (makeSimpleMove s) $ genBishopSquares s b

genBishopSquares :: Square -> Board -> [Square]
genBishopSquares s b = concat $ mkRaysFromBranches s b
  mini_bishop_branches

genQueenMoves :: Square -> Board -> [Move]
genQueenMoves sq bd = map (makeSimpleMove sq) $ genQueenSquares sq bd

genQueenSquares :: Square -> Board -> [Square]
genQueenSquares sq bd = genRookSquares sq bd ++ genBishopSquares sq bd

genKingSimpleSquares :: Square -> Board -> [Square]
genKingSimpleSquares s b = genSquaresFromBranches s b mini_rook_branches
