module Moves where

import Data.Char
import Data.Maybe
import Squares
import Pieces
import Parsing

data Move = Move {
  initSquare:: Maybe Square,
  finalSquare:: Square,
  movedPiece:: Maybe PieceType,
  promotedPiece:: Maybe(PieceType)
} deriving (Eq,Show)

showMove:: Move -> String
showMove mv = (if mp == Just (Pawn) then "" else show $ mp) ++
  (init) ++ (final) ++ (if promo == Nothing then "" else
  (show $ fromJust $ promo))
  where init  = map toLower $ show $ initSquare mv
        final = map toLower $ show $ finalSquare mv
        mp    = movedPiece mv
        promo = promotedPiece mv

pLf:: Parser ()
pLf = do many (sat (== '\n'))
         return ()

pMove :: Parser (Maybe Move)
pMove = do
          space <|> pLf
          sqi <- pSquare 
          sqf <- pSquare
          return (Just $ Move sqi (fromJust sqf) Nothing Nothing)

readMove:: String ->  (Maybe Move)
readMove str = fst $ head $ parse pMove str
