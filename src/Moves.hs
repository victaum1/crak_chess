module Moves where

import Data.Char
import Squares
import Pieces
import Parsing

data Move = Move {
  initSquare :: Square,
  finalSquare :: Square,
  promotedPiece :: Maybe PieceType
} deriving (Eq,Show)

showMove :: Move -> String
showMove mv = initM ++ final ++ maybe "" show promo
  where initM  = map toLower $ show $ initSquare mv
        final = map toLower $ show $ finalSquare mv
        promo = promotedPiece mv

pLf :: Parser ()
pLf = do many (sat (== '\n'))
         return ()

pMoveCoord :: Parser (Maybe Move)
pMoveCoord = do
          space <|> pLf
          sqi <- pSquare
          sqf <- pSquare
          return $ Just $ Move sqi sqf Nothing

readMove :: String -> Maybe Move
readMove str = fst $ head $ parse pMoveCoord str

