module Play where

-- import Game
-- import Board
-- import Pieces
-- import Data.Maybe
-- import qualified Data.Map.Strict as Map
-- import Moves
-- import Squares
-- import Rules

import Game
    ( GameState(castleFlag, nMoves, nPlys, turn, board, epSquare), Game, game2FEN )
import Board ( checkSquare, Board )
import Pieces ( Piece(Piece, pieceType), PieceType(Rook, Pawn), Side )
import Data.Maybe ( isJust, fromJust, isNothing, mapMaybe)
import qualified Data.Map.Strict as Map
import Moves ( readMove, Move(getCrown, getDestSq, getInitSq) )
import Squares ( Square(..), File, readSquare )
import Rules ( sameSide )


castle_moves = map (fromJust.readMove) ["e1g1","e1c1","e8g8", "e8c8"]
castle_rook_moves = map (fromJust.readMove) ["h1f1","a1d1","h8f8", "a8d8"]

flag_masks  = [-3,-3,-12,-12]
rook_flags  = [-1,-2,-4,-8]

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
  let o_nplys = if isPawn initSq i_bd || isCapture || isCrown then 0 else
        i_nplys + 1
  let o_nMoves = if not i_side then i_nMoves + 1 else i_nMoves
  if isCastle then do
    mask <- Map.lookup m castle_flags_map
    let o_castle = i_castle + mask
    o_bd <- mkMoveBoard m i_bd
    fun <- Map.lookup m castle_rook_map
    o_bd <- fun o_bd
    return (g{board=o_bd, turn = not i_side, nPlys = o_nplys
           , nMoves = o_nMoves, castleFlag=o_castle})
    else do
      if isCastleSq && isRookMove && i_castle /= 0 then do
        mask <- Map.lookup initSq rook_move_map
        let o_castle = i_castle + mask
        o_bd <- mkMoveBoard m i_bd
        return (g{board=o_bd, turn = not i_side, nPlys = o_nplys
           , nMoves = o_nMoves, castleFlag=o_castle})
        else do
        o_bd <- mkMoveBoard m i_bd
        let epSq = if isEpTrigged si m i_bd then mkEp si iFile else Nothing
        return (g{board=o_bd, turn = not i_side, nPlys = o_nplys
           , nMoves = o_nMoves, epSquare = epSq})
  where i_bd = if not isCrown then board g
          else Map.insert initSq (Piece (turn g) (fromJust crown_p))
            (board g)
        initSq = getInitSq m
        destSq = getDestSq m
        isCapture = isJust $ checkSquare destSq i_bd
        isCastle = m `elem` castle_moves
        isCastleSq = getInitSq m `elem` rook_castle_sqs
        isRookMove = Just Rook ==
         (pieceType <$> checkSquare initSq i_bd)
        crown_p = getCrown m
        isCrown = isJust crown_p
        iFile = squareFile initSq
        si = turn g

mkEp :: Side -> File -> Maybe Square
mkEp si fi | si = Just (Square fi 2)
           | otherwise = Just (Square fi 5)

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
  let destsq   = getDestSq m
  initpiece <- checkSquare initsq b
  let o_bd = Map.delete destsq (Map.delete initsq b)
  return (Map.insert destsq initpiece o_bd)
