module Play where

-- import Game
-- import Board
-- import Pieces
-- import Data.Maybe
-- import qualified Data.Map.Strict as Map
-- import Moves

import Game
import Board
import Pieces
import Data.Maybe
import qualified Data.Map.Strict as Map
import Moves
import Rules


makeMove :: Move -> Game -> Maybe Game
makeMove m g = do
  let i_side = turn g
  let i_nplys = nPlys g
  let i_nMoves = nMoves g
  let o_nplys = if isPawn || isCapture then 0 else i_nplys + 1
  let o_nMoves = if not i_side then i_nMoves + 1 else i_nMoves
  o_bd <- mkMoveBoard m i_bd
  return (g{board=o_bd, turn = not i_side, nPlys = o_nplys
           , nMoves = o_nMoves})
  where isPawn = Just Pawn == (pieceType <$> checkSquare (getInitSq m) i_bd)
        i_bd = board g
        isCapture = isJust $ checkSquare (getDestSq m) i_bd

mkMoveBoard :: Move -> Board -> Maybe Board
mkMoveBoard m b = do
  let initsq    = getInitSq m
  let desq      = getDestSq m
  initpiece <- checkSquare initsq b
  let a_bd      = Map.delete initsq b
  let destpiece = checkSquare desq b
  if isNothing destpiece then
    return (Map.insert desq initpiece a_bd)
    else
      if fromJust (sameSide <$> destpiece <*> Just initpiece)
        then Nothing else return (Map.insert desq initpiece a_bd)
