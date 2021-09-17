module Generator where
import Game
import Board
import Pieces
import Rules
import Squares
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moves
import Data.Bifunctor
import Data.Bits


moveGenerator :: Game -> [Move]
moveGenerator g = genKingMoves g ++ moveGenBySide g


moveGenBySide_ :: Side -> Board -> [Move]
moveGenBySide_ s b | s = concat $ map (`genKnightMoves` b) ns ++
  map (`genBishopMoves` b) bs ++
  map (`genRookMoves` b) rs ++
  map (`genQueenMoves` b) qs
                | otherwise = concat $ map (`genKnightMoves` b) ns ++
  map (`genBishopMoves` b) bs ++
  map (`genRookMoves` b) rs ++
  map (`genQueenMoves` b) qs
  where ns = whereIsPiece (Piece s Knight) b
        bs = whereIsPiece (Piece s Bishop) b
        rs = whereIsPiece (Piece s Rook) b
        qs = whereIsPiece (Piece s Queen) b

moveGenBySide :: Game -> [Move]
moveGenBySide g | s         = concat $ map (`genAllPawnMoves` g) wps ++
                  [moveGenBySide_ s b]
                | otherwise = concat $ map (`genAllPawnMoves` g) bps ++
                  [moveGenBySide_ s b]
  where s = turn g
        b = board g
        wps = whereIsPiece (Piece True Pawn) b
        bps = whereIsPiece (Piece False Pawn) b


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
  (mapMaybe (`checkSquare` bd) (genKnightSquares' sq bd))
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

squareAttackedByKing :: Side -> Board -> Square -> Bool
squareAttackedByKing si bd sq = sq `elem`
  genKingSimpleSquares' ksq bd
  where ksq = whereIsKing si bd

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

genAllPawnMoves :: Square -> Game -> [Move]
genAllPawnMoves sq gm = genEnPassant sq gm ++ crowns ++ ncm
  where gpm = genPawnMoves si sq bd
        si  = turn gm
        bd  = board gm
        ncm = filter (not . (`isMoveCrown` bd)) gpm
        cm  = filter (`isMoveCrown` bd) gpm
        crowns = concatMap (`genCrown` bd) cm

genEnPassant :: Square -> Game -> [Move]
genEnPassant sq gm | notEp = []
                   | otherwise = map (makeSimpleMove sq) genSq
  where notEp = isNothing $ epSquare gm
        ep = fromJust $ epSquare gm
        si = turn gm
        bd = board gm
        genSq = filter (==ep) $ genPawnCaptureSquares si sq bd

genCrown :: Move -> Board -> [Move]
genCrown m bd = map (makeCrownMove isq fsq) pTs
  where pTs = [Queen,Rook,Knight,Bishop]
        isq = getInitSq m
        fsq = getDestSq m

isMoveCrown :: Move -> Board -> Bool
isMoveCrown (Move isq fsq _) bd | isPawn && si = squareRank isq == 6 &&
                                  squareRank fsq == 7
                                | isPawn && not si = squareRank isq == 1 &&
                                  squareRank fsq == 0
                                | otherwise = False
  where isPawn = Pawn == maybe King pieceType (checkSquare isq bd)
        si = maybe False pieceSide (checkSquare isq bd)


makeSquares' :: (Int,Int) -> [CPath] -> [(Int,Int)]
makeSquares' tsq = map $ compose tsq

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
  b) (genSquaresFromBranches' s b c)


genSquaresFromBranches' :: Square -> Board -> CBranch -> [Square]
genSquaresFromBranches' s b c = map tuple2Square (filter onBoard' (makeSquares'
  (square2Tuple s) c))

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
makeSimpleMove i f = Move i f Nothing


makeCrownMove :: Square -> Square -> PieceType -> Move
makeCrownMove i f p = Move i f (Just p)


genKnightMoves :: Square -> Board -> [Move]
genKnightMoves s b = genMoveFromBranches s b knight_branches

genKnightSquares :: Square -> Board -> [Square]
genKnightSquares s b = genSquaresFromBranches s b knight_branches

genKnightSquares' :: Square -> Board -> [Square]
genKnightSquares' s b = genSquaresFromBranches' s b knight_branches

genRookMoves :: Square -> Board -> [Move]
genRookMoves s b = map (makeSimpleMove s) $ filter
  (not . sameSideStep s b) (genRookSquares s b)

genRookSquares :: Square -> Board -> [Square]
genRookSquares s b = concat $ mkRaysFromBranches s b
  mini_rook_branches

genBishopMoves :: Square -> Board -> [Move]
genBishopMoves s b = map (makeSimpleMove s) $ filter
  (not . sameSideStep s b) (genBishopSquares s b)

genBishopSquares :: Square -> Board -> [Square]
genBishopSquares s b = concat $ mkRaysFromBranches s b
  mini_bishop_branches


genQueenMoves :: Square -> Board -> [Move]
genQueenMoves s b = map (makeSimpleMove s) $ filter
  (not . sameSideStep s b) $ genQueenSquares s b

genQueenSquares :: Square -> Board -> [Square]
genQueenSquares s b = genRookSquares s b ++ genBishopSquares s b


genKingSimpleSquares :: Square -> Board -> [Square]
genKingSimpleSquares s b = genSquaresFromBranches s b (mini_rook_branches
  ++ mini_bishop_branches)


genKingSimpleSquares' :: Square -> Board -> [Square]
genKingSimpleSquares' s b = genSquaresFromBranches' s b (mini_rook_branches
  ++ mini_bishop_branches)

genCastleSquare :: Side -> Bool -> Square
genCastleSquare s b | s && b = Square 6 0
                    | s && not b = Square 2 0
                    | not s && b = Square 6 7
                    | otherwise = Square 2 7

genCastleSquares :: Game -> [Square]
genCastleSquares g | isInCheck g = []
                   | otherwise = map (genCastleSquare s) $ filter
                      (checkCastleSquares g) $ filter (isFlagCastle g) sd
  where s = turn g
        sd = [True,False] -- King , Queen side...

genKingMoves :: Game -> [Move]
genKingMoves g =  map (makeSimpleMove kSq) (
  filter (not . squareAttackedByKing (not s) b)
  (filter (not . (`squareAttack` g)) (genKingSimpleSquares kSq b))) ++ map (makeSimpleMove  kSq) (genCastleSquares g)
  where b = board g
        s = turn g
        kSq = head $ whereIsPiece (Piece s King) b

isFlagCastle :: Game -> Bool -> Bool
isFlagCastle g b | s =  if b then 1 .&. c > 0 else 2 .&. c > 0
                 | otherwise = if b then 4 .&. c > 0 else 8 .&. c > 0
  where s = turn g
        c = castleFlag g




checkCastleSquares :: Game -> Bool -> Bool
checkCastleSquares g b | s && b     = (((==2) . length) . filterBoth)
  whiteKingSide
                       | s && not b = (((==2) . length) . filterBoth)
  whiteQueenSide && isEmpty (Square 1 0) bd
                       | not s && b = (((==2) . length) . filterBoth)
  blackKingSide
                       | otherwise  = (((==2) . length) . filterBoth)
  blackQueenSide && isEmpty (Square 1 7) bd

  where
       whiteKingSide  = [Square 5 0, Square 6 0]
       whiteQueenSide = [Square 3 0, Square 2 0]
       blackKingSide  = [Square 5 7, Square 6 7]
       blackQueenSide = [Square 3 7, Square 2 7]
       s = turn g
       bd = board g
       filterBoth = filter (not .
          (`squareAttack` g)) . filter (`isEmpty` bd)


isInCheck :: Game -> Bool
isInCheck g = isBoardInCheck s b
  where b = board g
        s = turn g

isBoardInCheck :: Side -> Board -> Bool
isBoardInCheck s b = squareAttackOnBoard ns (whereIsKing s b)
  b
  where ns = not s

isCapture :: Board -> Move -> Bool
isCapture bd mv = isJust mp
 where (Move _ dsq _) = mv
       mp = checkSquare dsq bd
