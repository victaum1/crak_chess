module Play where

import Game
import Board
import Pieces
import Data.Maybe
import qualified Data.Map.Strict as Map
import Moves
import Squares
import Rules
import Data.Bits


castle_moves = map (fromJust.readMove) ["e1g1","e1c1","e8g8", "e8c8"]
castle_rook_moves = map (fromJust.readMove) ["h1f1","a1d1","h8f8", "a8d8"]

flag_masks  = [3,3,12,12]
rook_flags  = [1,2,4,8]

castle_flags_map = Map.fromList $ zip castle_moves flag_masks

castle_map = Map.fromList $ zip castle_moves (map mkSimpleMoveBoard castle_rook_moves)

castle_rook_map = Map.fromList $ zip castle_moves (map mkSimpleMoveBoard
  castle_rook_moves)

rook_castle_sqs = map (square2Tuple.fromJust.readSquare) ["h1","a1","h8","a8"]

rook_move_map = Map.fromList $ zip rook_castle_sqs rook_flags

makeMove :: Move -> Game -> Maybe Game
makeMove m g | isNullMove m = Just (MkGame (bd,
                                            not (turn g),cf,Nothing
                                           ,nPlys g + 1,if turn g then
                                               nMoves g else nMoves g + 1))
             | otherwise = do
  let i_side = si
  let i_nplys = nPlys g
  let i_nMoves = nMoves g
  let i_castle = cf
  let i_bd = if not isCrown then bd
          else MkBoard (Map.insert initSq (turn g,fromJust $ fromPtype
                                            crown_p) (fromBoard (board g)))
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
          (isKingOutOfInitSq (not si)) = if si && destSq == MkSquare (7,7) then
              i_castle `clearBit` 2
            else if si && destSq == MkSquare (0,7) then
              i_castle `clearBit` 3 else
            if not si && destSq == MkSquare (7,0) then
              i_castle `clearBit` 0 else
               if not si && destSq == MkSquare (0,0) then
                 i_castle `clearBit` 1
                            else i_castle
        | otherwise = i_castle
  let o_nplys = if isPawn (tuple2Square initSq) i_bd || isCapture || isCrown then 0 else
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
           mkMoveBoard m (MkBoard (Map.delete (square2Tuple (epSqDel (not si) dFile)) (fromBoard i_bd)))
           else mkMoveBoard m i_bd
  return (MkGame (o_bd,o_side,o_castle,epSq,o_nplys,o_nMoves))
  where initSq = square2Tuple (getInitSq m)
        destSq = getDestSq m
        isCapture = isJust $ checkSquare destSq bd
        isCastle = m `elem` castle_moves && isKingMove
        isRookCaptureInCastleSq = destSq `elem` map tuple2Square rook_castle_sqs
        isRookMovedFromCastleSq = initSq `elem` rook_castle_sqs
        isRookMove = Just Rook ==
         (pieceType <$> checkSquare (tuple2Square initSq) bd)
        isKingMove = Just King ==
         (pieceType <$> checkSquare (tuple2Square initSq) bd)
        crown_p = getCrown m
        isCrown = isJust (fromPtype crown_p)
        iFile = squareFile (tuple2Square initSq)
        dFile = squareFile destSq
        si = turn g
        bd = board g
        cf = castleFlag g
        isKingOutOfInitSq ss = if ss then
          whereIsKing ss /= [MkSquare (4, 0)] else
          whereIsKing ss /= [MkSquare (4,7)]
        whereIsKing ss = whereIsPiece (MkPiece (ss,King)) bd

isKingMoved :: Move -> Game -> Bool
isKingMoved m g = Just King == (pieceType <$> checkSquare sq bd)
  where sq = getInitSq m
        bd = board g

isEpCapture :: Move -> Game -> Bool
isEpCapture m g = isSidePawn && Just destSq==epSq
  where initSq = getInitSq m
        destSq = getDestSq m
        bd = board g
        si = turn g
        epSq = epSquare g
        isSidePawn = Just (MkPiece (si,Pawn)) == checkSquare initSq bd

mkEp :: Side -> File -> Maybe Square
mkEp si fi | si = Just (MkSquare (fi,2))
           | otherwise = Just (MkSquare (fi,5))

epSqDel :: Side -> File -> Square
epSqDel si fi | si = MkSquare (fi,3)
              | otherwise = MkSquare (fi,4)

isEpTrigged :: Side -> Move -> Board -> Bool
isEpTrigged si mv bd | isDoublePushPawn mv si bd = MkPiece (not si,Pawn)
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
adjSquares si (MkSquare (f,r)) | si = [MkSquare (f+1,r+2), MkSquare (f-1,r+2)]
                           | otherwise =
                             [MkSquare (f+1,r-2), MkSquare (f-1,r-2)]

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
  let initsq = square2Tuple (getInitSq m)
  let destsq = square2Tuple (getDestSq m)
  initpiece <- fromPiece <$> checkSquare (tuple2Square initsq) b
  let o_bd = Map.delete destsq (Map.delete initsq (fromBoard b))
  return (MkBoard (Map.insert destsq initpiece o_bd))
