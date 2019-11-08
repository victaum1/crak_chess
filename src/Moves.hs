module Moves where

import Data.Char
import Data.Maybe
import Squares
import Pieces
import Parsing

data Move = Move {
  initSquare:: Square,
  finalSquare:: Square,
  movedPiece::  Maybe PieceType,
  capturedPiece:: Maybe PieceType,
  promotedPiece:: Maybe PieceType
} deriving (Eq,Show)

showMove:: Move -> String
showMove mv = (if mp == Just Pawn then "" else maybe "" show mp) ++
  init ++ final ++ maybe "" show promo
  where init  = map toLower $ show $ initSquare mv
        final = map toLower $ show $ finalSquare mv
        mp    = movedPiece mv
        promo = promotedPiece mv

pLf:: Parser ()
pLf = do many (sat (== '\n'))
         return ()

pMoveCoord :: Parser (Maybe Move)
pMoveCoord = do
          space <|> pLf
          sqi <- pSquare 
          sqf <- pSquare
          return $ Just $ Move (fromJust sqi) (fromJust sqf) Nothing Nothing
            Nothing

readMove:: String -> Maybe Move
readMove str = fst $ head $ parse pMoveCoord str
