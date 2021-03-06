{-# LANGUAGE LambdaCase #-}
module Pieces (piece_types, piece_chars, Side(..), Piece(..)
  , showPiece, readCPiece, PieceType(..), board_piece_chars, pPiece
  , pPieceType, all_piece_chars)
  where

import Parsing
import Data.Char  (isUpper, toLower, toUpper)
import Data.Maybe ( fromJust, isJust )

-- adts
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq,Ord,Enum)

instance Show PieceType where
  show a | a == Pawn   = "P"
         | a == Knight = "N"
         | a == Bishop = "B"
         | a == Rook   = "R"
         | a == Queen  = "Q"
         | a == King   = "K"

data Side =  White | Black
             deriving (Eq,Ord)

instance Show Side where
  show s | s == White = "W" -- First Team
         | otherwise  = "B" -- The other Team

data Piece = Piece {
                      pieceSide :: Side
                    , pieceType :: PieceType
                    } deriving (Eq,Ord)

instance Show Piece where
  show (Piece s p) = show s ++ show p

-- vars / const
piece_types = [Pawn .. ]
piece_chars = "PNBRQK"
l_piece_chars = map toLower piece_chars
all_piece_chars = piece_chars ++ l_piece_chars
board_piece_chars = "." ++ all_piece_chars

type_map = zip piece_chars piece_types
type_map' = zip piece_types piece_chars

-- funcs
readPieceType :: Char -> Maybe PieceType
readPieceType c = lookup c type_map

readCPiece :: Char -> Maybe Piece
readCPiece c | toUpper c `elem` piece_chars = Just(Piece (toSide c)
               (toPt c))
             | otherwise = Nothing
           where
             toPt x = fromJust $ readPieceType (toUpper c)
             toSide x = if isUpper x then White else Black

showPiece :: Piece -> Char
showPiece p = if pieceSide p == White then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) type_map'


pPiece :: Parser Piece
pPiece = P(\case
  [] -> []
  (r:cs) -> [(fromJust (readCPiece r),cs)|isJust $ readCPiece r]
          )

pPieceType :: Parser (Maybe PieceType)
pPieceType = P(\case
  [] -> []
  (r:cs) -> [(readPieceType r, cs)])
 
