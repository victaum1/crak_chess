{-# LANGUAGE LambdaCase #-}
module Pieces
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

type Side = Bool

newtype Ptype = Ptype {fromPtype :: Maybe PieceType} deriving (Eq, Ord)

instance Show Ptype where
  show (Ptype Nothing)  = ""
  show (Ptype (Just a)) = show a

type Tpiece = (Side,PieceType)

newtype Piece = MkPiece {fromPiece::Tpiece} deriving (Eq,Ord)

instance Show Piece where
  show (MkPiece (s,p)) = (if s then "W" else "B") ++ show p


-- vars / const
piece_types =  [Pawn .. ]
all_pieces = zipWith (curry (MkPiece)) (replicate 6 True
  ++ replicate 6 False) (concat $ replicate 2 piece_types)
  
piece_chars = "PNBRQK"
l_piece_chars = map toLower piece_chars
all_piece_chars = piece_chars ++ l_piece_chars
board_piece_chars = "." ++ all_piece_chars

type_map = zip piece_chars piece_types
type_map' = zip piece_types piece_chars

all_black_pieces = map (MkPiece) $ zip (replicate 6 False) piece_types
all_white_pieces = map (MkPiece) $ zip (replicate 6 True) piece_types


-- funcs
pieceSide :: Piece -> Side
pieceSide (MkPiece (s,_)) = s

pieceType :: Piece -> PieceType
pieceType (MkPiece (_,pt)) = pt

readPieceType :: Char -> Ptype
readPieceType c = Ptype (lookup (toUpper c) type_map)

readCPiece :: Char -> Maybe Piece
readCPiece c | toUpper c `elem` piece_chars = Just(MkPiece
                                                   (isUpper c,toPt c))

             | otherwise = Nothing
           where
             toPt x = fromJust $ fromPtype $ readPieceType (toUpper c)

showPiece :: Piece -> Char
showPiece p = if pieceSide p then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) type_map'


pPiece :: Parser Piece
pPiece = P(\case
  [] -> Nothing
  (r:cs) -> (\p -> Just (p,cs)) =<< readCPiece r)


pPieceType :: Parser Ptype
pPieceType = P(\case
  [] -> Nothing
  (r:cs) -> Just (readPieceType r, cs))
 
