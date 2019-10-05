module Pieces (
  PieceType(..),
  Side(..),
  Piece(..),
  showPiece,
  readPiece,
  pPieceType
  ) where

import Data.Maybe
import Data.Char (toUpper, toLower, isUpper)
import Parsing

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
             deriving (Eq)

instance Show PieceType where
  show a | a == Pawn   = "P"
         | a == Knight = "N"
         | a == Bishop = "B"
         | a == Rook   = "R"
         | a == Queen  = "Q"
         | a == King   = "K"

data Side =  White | Black
             deriving (Eq, Show)

data Piece = Piece {
                      pieceSide  :: Side
                    , pieceType  :: PieceType
                    } deriving (Eq)

instance Show Piece where
  show (Piece a b) = show a ++ show b

pieceTypeList = [Pawn,Knight,Bishop,Rook,Queen,King]
pieceCharList = "PNBRQK"

typeList = zip pieceCharList pieceTypeList
typeList' = zip pieceTypeList pieceCharList

readPieceType :: Char -> Maybe PieceType
readPieceType c = if elem (toUpper c) pieceCharList
  then Just(toPt c) else Nothing
  where toPt x = fromJust $ lookup (toUpper x) typeList

readPiece :: Char -> Maybe Piece
readPiece c =  if elem (toUpper c) pieceCharList
  then Just(Piece (toSide c) (toPt c)) else Nothing
  where
    toPt x = fromJust $ lookup (toUpper x) typeList
    toSide x = if isUpper x then White else Black

showPiece :: Piece -> Char
showPiece p = if pieceSide p == White then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) typeList'

pPieceType :: Parser (Maybe PieceType)
pPieceType = P(
               \inp -> case inp of
                 []  -> []
                 (c:cs) -> if isUpper c then [(toPt c,cs++" ")]
                   else [(Nothing,c:(cs++" "))]
               )
               where toPt = readPieceType
