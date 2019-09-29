module Moves where

import Squares
import Pieces

data Move = Move {
  initSquare:: Square,
  finalSquare:: Square,
  movedPiece:: PieceType,
  promotedPiece:: Maybe(PieceType)
} deriving (Eq,Show)

showMove:: Move -> String
showMove mv = "e2e4" -- TODO

readMove:: String -> Move
-- TODO
readMove str = Move (Square E_F R2) (Square E_F R4) Pawn Nothing
