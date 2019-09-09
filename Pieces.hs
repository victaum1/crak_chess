module Pieces (Ptype(..),Side(..), Piece(..)) where

data Ptype = Pawn | Knight | Bishop | Rook | Queen | King
             deriving (Eq, Show)

data Side =  White | Black
             deriving (Eq, Show)

data Piece = Piece {
                      pieceSide   :: Side
                    , pieceType  :: Ptype
                    } deriving (Eq)

instance Show Piece where
  show (Piece a b) = show a ++ show b

