module Pieces (pieceTypeList, pieceCharList, Side(..), Piece(..), showPiece
  , readCPiece)
  where

import Data.Maybe
import Data.Char (toUpper, toLower, isUpper)
import Parsing

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving Eq

instance Show PieceType where
  show a | a == Pawn   = "P"
         | a == Knight = "N"
         | a == Bishop = "B"
         | a == Rook   = "R"
         | a == Queen  = "Q"
         | a == King   = "K"

data Side =  White | Black
             deriving Eq

instance Show Side where
  show s | s == White = "W"
         | otherwise  = "B"

data Piece = Piece {
                      pieceSide  :: Side
                    , pieceType  :: PieceType
                    } deriving (Eq)

instance Show Piece where
  show (Piece a b) = show a ++ show b

pieceTypeList = [Pawn, Knight, Bishop, Rook, Queen, King]
pieceCharList = "PNBRQK"

typeList = zip pieceCharList pieceTypeList
typeList' = zip pieceTypeList pieceCharList

readCPiece :: Char -> Maybe (Maybe Piece)
readCPiece c | c == '.' = Just Nothing
             | elem (toUpper c) pieceCharList = Just(Just(Piece (toSide c) 
               (toPt c)))
             | otherwise = Nothing
           where
             toPt x = fromJust $ lookup (toUpper x) typeList
             toSide x = if isUpper x then White else Black
             
showPiece :: Piece -> Char
showPiece p = if pieceSide p == White then p2char p else
  (toLower . p2char) p
  where p2char x = fromJust $ lookup (pieceType x) typeList'

