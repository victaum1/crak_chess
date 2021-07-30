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
import Data.Bifunctor (bimap)


squareAttack :: Square -> Game -> Bool
squareAttack sq ge = squareAttackOnBoard (not aSide) sq aBoard
  where aSide = turn ge
        aBoard = board ge

squareAttackOnBoard :: Side -> Square -> Board -> Bool
squareAttackOnBoard si sq bd = squareAttackByKnight sq si bd ||
  squareAttackByBishop sq si bd || squareAttackByRook sq si bd ||
  squareAttackByQueen sq si bd  || squareAttackByPawn sq si bd


squareAttackByKnight :: Square -> Side -> Board -> Bool
squareAttackByKnight sq si bd =  any isKnight
  (mapMaybe (`checkSquare` bd) (genKnightSquares sq Map.empty))
  where isKnight p = Piece si Knight == p


squareAttackByQueen :: Square -> Side -> Board -> Bool
squareAttackByQueen sq si bd =  any isQueen
  (mapMaybe (`checkSquare` bd) (genQueenSquares sq bd))
  where isQueen p = Piece si Queen == p

squareAttackByBishop :: Square -> Side -> Board -> Bool
squareAttackByBishop sq si bd =  any isBishop
  (mapMaybe (`checkSquare` bd) (genBishopSquares sq bd))
  where isBishop p = Piece si Bishop == p


squareAttackByRook :: Square -> Side -> Board -> Bool
squareAttackByRook  sq si bd =  any isRook
  (mapMaybe (`checkSquare` bd) (genRookSquares sq bd))
  where isRook p = Piece si Rook == p


squareAttackByPawn :: Square -> Side -> Board -> Bool
squareAttackByPawn sq si bd = any isPawn
  (mapMaybe (`checkSquare` bd) (genPawnCaptureSquares (not si) sq
     Map.empty))
  where isPawn p = Piece si Pawn == p


genPawnMoves :: Side -> Square -> Board -> [Move]
genPawnMoves si sq bd = genPawnCaptures si sq bd ++
  genPawnStepMoves si sq bd

genPawnCaptures :: Side -> Square -> Board -> [Move]
genPawnCaptures si sq bd = map (makeSimpleMove sq) $ filter
  (not . (`isEmpty` bd)) $ filter (not . sameSideStep sq bd)
  $ genPawnCaptureSquares si sq bd


genPawnCaptureSquares :: Side -> Square -> Board -> [Square]
genPawnCaptureSquares si sq bd | si = genSquaresFromBranches sq bd
  white_pawn_captures
                               | otherwise = genSquaresFromBranches sq bd
  black_pawn_captures


genPawnStepMoves :: Side -> Square -> Board -> [Move]
genPawnStepMoves si sq bd = map (makeSimpleMove sq) (filter (`isEmpty` bd)
  (genPawnStepSquares si sq bd))


genPawnStepSquares :: Side -> Square -> Board -> [Square]
genPawnStepSquares si sq bd | si = if is1Rank then
                                     map tuple2Square $
                                       mkRayIter sq bd (head
                                         white_pawn_step) 2 []
                                   else
                                     genSquaresFromBranches sq bd
                                       white_pawn_step
                            | otherwise = if is1Rank then
                                     map tuple2Square $
                                       mkRayIter sq bd (head
                                         black_pawn_step) 2 []
                                   else
                                     genSquaresFromBranches sq bd
                                       black_pawn_step
  where is1Rank | si = 1 == squareRank sq
                | otherwise = 6 == squareRank
                  sq


moveGenerator :: Game -> [Move]
moveGenerator _ = []


makeSquares' :: (Int,Int) -> [CPath] -> [(Int,Int)]
makeSquares' sq = map $ compose sq


makeSquares :: Square -> [CPath] -> [Square]
makeSquares sq ds = map tuple2Square (makeSquares' (square2Tuple sq) ds)


compose :: (Int,Int) -> [Dir] -> (Int,Int)
compose inp ds = addTuple (foldr orthoMove (0,0) ds)
  where addTuple f = bimap (fst f +) (snd f +) inp
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
mkRaysFromBranches sq bd = map mkRay
  where mkRay di = map tuple2Square (mkRayIter sq bd di 7 [])


mkRayIter :: Square -> Board -> CPath -> Int -> [(Int,Int)] -> [(Int,Int)]
mkRayIter _ _ _ 0 isq = filter onBoard' isq
mkRayIter sq bd dr n [] = mkRayIter sq bd dr (n - 1)
  (makeSquares' (square2Tuple sq) [dr])
mkRayIter sq bd dr n isq | onBoard' (head isq) && isEmpty (tuple2Square $
  head isq) bd = mkRayIter sq bd dr (n - 1) (makeSquares' (head isq) [dr])
                 ++ isq
                         | onBoard' (head isq) && not (isEmpty
  (tuple2Square $ head isq) bd) = isq
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
genRookMoves s b = map (makeSimpleMove s) $ filter
  (not . sameSideStep s b) (genRookSquares s b)

genRookSquares :: Square -> Board -> [Square]
genRookSquares s b = concat $ mkRaysFromBranches s b
  mini_rook_branches

genBishopMoves :: Square -> Board -> [Move]
genBishopMoves s b = map (makeSimpleMove s) $ filter
  (sameSideStep s b) (genBishopSquares s b)

genBishopSquares :: Square -> Board -> [Square]
genBishopSquares s b = concat $ mkRaysFromBranches s b
  mini_bishop_branches

genQueenMoves :: Square -> Board -> [Move]
genQueenMoves sq bd = map (makeSimpleMove sq) $ genQueenSquares sq bd

genQueenSquares :: Square -> Board -> [Square]
genQueenSquares sq bd = genRookSquares sq bd ++ genBishopSquares sq bd

genKingSimpleSquares :: Square -> Board -> [Square]
genKingSimpleSquares s b = genSquaresFromBranches s b (mini_rook_branches
  ++ mini_bishop_branches)
