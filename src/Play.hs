module Play where

import Game
    ( GameState(epSquare, castleFlag, nMoves, nPlys, board, turn),
      Game )
import Board ( whereIsPiece, checkSquare, Board )
import Pieces
    ( PieceType(Pawn, King, Rook), Piece(Piece, pieceType), Side )
import Data.Maybe ( isJust, fromJust, isNothing, mapMaybe )
import qualified Data.Map.Strict as Map
import Moves ( readMove, Move(getCrown, getDestSq, getInitSq) )
import Squares ( File, readSquare, Square(..) )
import Rules ( sameSide )
import Data.Bits ( Bits(clearBit, complement, (.&.)) )

castle_moves = map (fromJust.readMove) ["e1g1","e1c1","e8g8", "e8c8"]
castle_rook_moves = map (fromJust.readMove) ["h1f1","a1d1","h8f8", "a8d8"]

flag_masks  = [3,3,12,12]
rook_flags  = [1,2,4,8]

castle_flags_map = Map.fromList $ zip castle_moves flag_masks

castle_map = Map.fromList $ zip castle_moves (map mkSimpleMoveBoard castle_rook_moves)

castle_rook_map = Map.fromList $ zip castle_moves (map mkSimpleMoveBoard
  castle_rook_moves)

rook_castle_sqs = map (fromJust.readSquare) ["h1","a1","h8","a8"]

rook_move_map = Map.fromList $ zip rook_castle_sqs rook_flags

makeMove :: Move -> Game -> Maybe Game
makeMove m g = do
  let i_side = turn g
  let i_nplys = nPlys g
  let i_nMoves = nMoves g
  let i_castle = castleFlag g
  let i_bd = if not isCrown then board g
          else Map.insert initSq (Piece (turn g) (fromJust crown_p))
            (board g)
  let o_side = not i_side
  let o_castle
        | isCastle = maybe i_castle ((.&.) i_castle . complement)
          (Map.lookup m castle_flags_map)
        | isRookMovedFromCastleSq && isRookMove && i_castle /= 0 && not
          (isKingOutOfInitSq si) = maybe i_castle ((.&.) i_castle . complement)
          (Map.lookup initSq rook_move_map)
       | isKingMoved m g && i_castle /=0 && not (isKingOutOfInitSq si) =
          if i_side then i_castle .&. 12
          else i_castle .&. 3
        | isRookCaptureInCastleSq && i_castle /=0 && not
          (isKingOutOfInitSq (not si)) = if si && destSq == Square 7 7 then
              i_castle `clearBit` 2
            else if si && destSq == Square 0 7 then 
              i_castle `clearBit` 3 else
            if not si && destSq == Square 7 0 then
              i_castle `clearBit` 0 else
               if not si && destSq == Square 0 0 then
                 i_castle `clearBit` 1
                            else i_castle
        | otherwise = i_castle
  let o_nplys = if isPawn initSq i_bd || isCapture || isCrown then 0 else
        i_nplys + 1
  let o_nMoves = if not i_side then i_nMoves + 1 else i_nMoves
  let epSq =
        if isEpTrigged si m i_bd then mkEp si iFile else Nothing
  o_bd <-
    if isCastle then do
      fun <- Map.lookup m castle_rook_map
      i_bd_ <- mkMoveBoard m i_bd
      fun i_bd_
    else if isEpCapture m g then
           mkMoveBoard m (Map.delete (epSqDel (not si) dFile) i_bd)
           else mkMoveBoard m i_bd
  return g{board=o_bd,turn=o_side,castleFlag=o_castle,epSquare=epSq,
           nPlys=o_nplys,nMoves=o_nMoves}
  where initSq = getInitSq m
        destSq = getDestSq m
        isCapture = isJust $ checkSquare destSq bd
        isCastle = (m `elem` castle_moves) && isKingMove
        isRookCaptureInCastleSq = destSq `elem` rook_castle_sqs
        isRookMovedFromCastleSq = initSq `elem` rook_castle_sqs
        isRookMove = Just Rook ==
         (pieceType <$> checkSquare initSq bd)
        isKingMove = Just King ==
         (pieceType <$> checkSquare initSq bd)
        crown_p = getCrown m
        isCrown = isJust crown_p
        iFile = squareFile initSq
        dFile = squareFile destSq
        si = turn g
        bd = board g
        isKingOutOfInitSq ss = if ss then whereIsKing ss /= [Square 4 0] else
          whereIsKing ss /= [Square 4 7]
        whereIsKing ss = whereIsPiece (Piece ss King) bd


isKingMoved :: Move -> Game -> Bool
isKingMoved m g = Just King == (pieceType <$> checkSquare sq bd)
  where sq = getInitSq m
        bd = board g


isEpCapture :: Move -> Game -> Bool
isEpCapture m g = isSidePawn && (Just destSq==epSq)
  where initSq = getInitSq m
        destSq = getDestSq m
        bd = board g
        si = turn g
        epSq = epSquare g
        isSidePawn = Just (Piece si Pawn) == checkSquare initSq bd


mkEp :: Side -> File -> Maybe Square
mkEp si fi | si = Just (Square fi 2)
           | otherwise = Just (Square fi 5)


epSqDel :: Side -> File -> Square
epSqDel si fi | si = Square fi 3
              | otherwise = Square fi 4


isEpTrigged :: Side -> Move -> Board -> Bool
isEpTrigged si mv bd | isDoublePushPawn mv si bd = Piece (not si) Pawn
                       `elem` mapMaybe (`checkSquare` bd)
                       (adjSquares si initSq)
                     | otherwise = False
  where initSq = getInitSq mv

isPawn :: Square -> Board -> Bool
isPawn sq bd = Just Pawn == (pieceType <$> checkSquare sq bd)

isDoublePushPawn :: Move -> Side -> Board -> Bool
isDoublePushPawn m s b | isPawn initSq b = if s then delta > 1
                           else delta < -1
                       | otherwise = False
  where initSq = getInitSq m
        destSq = getDestSq m
        iRank  = squareRank initSq
        fRank  = squareRank destSq
        delta  = fRank - iRank

adjSquares :: Side -> Square -> [Square]
adjSquares si (Square f r) | si = [Square (f+1) (r+2), Square (f-1) (r+2)]
                           | otherwise =
                             [Square (f+1) (r-2), Square (f-1) (r-2)]

mkMoveBoard :: Move -> Board -> Maybe Board
mkMoveBoard m b = do
  let initsq    = getInitSq m
  let destsq      = getDestSq m
  let destpiece = checkSquare destsq b
  initpiece <- checkSquare initsq b
  if isNothing destpiece then
    mkSimpleMoveBoard m b
    else
      if fromJust (sameSide <$> destpiece <*> Just initpiece)
        then Nothing else
        mkSimpleMoveBoard m b


mkSimpleMoveBoard :: Move -> Board -> Maybe Board
mkSimpleMoveBoard m b = do
  let initsq = getInitSq m
  let destsq = getDestSq m
  initpiece  <- checkSquare initsq b
  let o_bd   = Map.delete destsq (Map.delete initsq b)
  return (Map.insert destsq initpiece o_bd)
