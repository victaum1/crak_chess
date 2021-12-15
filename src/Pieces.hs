{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Pieces (piece_types, piece_chars, Side(..), Piece(..)
  , showPiece, readCPiece, PieceType(..), board_piece_chars, pPiece
  , pPieceType, all_piece_chars, all_pieces)
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

-- data Side =  White | Black
--             deriving (Eq,Ord)

type Side = Bool

data Piece = Piece {
                      pieceSide :: Side
                    , pieceType :: PieceType
                    } deriving (Eq,Ord)

instance Show Piece where
  show (Piece s p) = (if s then "W" else "B") ++ show p

instance {-# OVERLAPS #-} Show (Maybe PieceType) where
  show Nothing = ""
  show (Just a) = show a


-- vars / const
piece_types = [Pawn .. ]
all_pieces = zipWith Piece (replicate 6 True ++ replicate 6 False) (concat
  $ replicate 2 piece_types)
  
piece_chars = "PNBRQK"
l_piece_chars = map toLower piece_chars
all_piece_chars = piece_chars ++ l_piece_chars
board_piece_chars = "." ++ all_piece_chars

type_map = zip piece_chars piece_types
type_map' = zip piece_types piece_chars

-- funcs
readPieceType :: Char -> Maybe PieceType
readPieceType c = lookup (toUpper c) type_map

readCPiece :: Char -> Maybe Piece
readCPiece c | toUpper c `elem` piece_chars = Just(Piece (isUpper c)
               (toPt c))
             | otherwise = Nothing
           where
             toPt x = fromJust $ readPieceType (toUpper c)
             -- toSide x = if isUpper x then White else Black

showPiece :: Piece -> Char
showPiece p = if pieceSide p then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) type_map'


pPiece :: Parser Piece
pPiece = P(\case
  [] -> Nothing
  (r:cs) -> (\p -> Just (p,cs)) =<< readCPiece r)


pPieceType :: Parser (Maybe PieceType)
pPieceType = P(\case
  [] -> Nothing
  (r:cs) -> Just (readPieceType r, cs))
 
