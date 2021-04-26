{-# LANGUAGE LambdaCase #-}
module Pieces (piece_type_list, piece_char_list, Side(..), Piece(..), showPiece
  , readCPiece, PieceType(..), piece_fen_list, pPiece, pPieceType)
  where

import Parsing ( Parser(..) )
import           Data.Char  (isUpper, toLower, toUpper)
import Data.Maybe ( fromJust, isJust )


-- vars / const
piece_type_list = [Pawn .. ]
piece_char_list = "PNBRQK"
piece_fen_list = "." ++ piece_char_list

type_list = zip piece_char_list piece_type_list
type_list' = zip piece_type_list piece_char_list


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
             deriving (Eq,Ord,Enum)

instance Show Side where
  show s | s == White = "W" -- First Team
         | otherwise  = "B" -- The other Team

data Piece = Piece {
                      pieceSide :: Side
                    , pieceType :: PieceType
                    } deriving (Eq,Ord)

instance Show Piece where
  show (Piece s p) = show s ++ show p


-- funcs
readPieceType :: Char -> Maybe PieceType
readPieceType c = lookup c type_list

readCPiece :: Char -> Maybe Piece
readCPiece c | toUpper c `elem` piece_char_list = Just(Piece (toSide c)
               (toPt c))
             | otherwise = Nothing
           where
             toPt x = fromJust $ readPieceType (toUpper c)
             toSide x = if isUpper x then White else Black

showPiece :: Piece -> Char
showPiece p = if pieceSide p == White then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) type_list'


pPiece :: Parser Piece
pPiece = P(\case
  [] -> []
  (r:cs) -> [(fromJust (readCPiece r),cs)|isJust $ readCPiece r]
          )


pPieceType :: Parser (Maybe PieceType)
pPieceType = P(\case
  [] -> []
  (r:cs) -> [(readPieceType r, cs)])
