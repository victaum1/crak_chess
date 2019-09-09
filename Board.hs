module Board (Pos(..), Board) where

import Pieces
import Squares

data Pos = Pos {getSquare :: Square, getPiece :: Piece}

instance Show Pos where
  show (Pos a b) = "(" ++ show a ++ "," ++ show b ++ ")"

type Board = [Pos]
