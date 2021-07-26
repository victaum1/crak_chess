{-# LANGUAGE FlexibleInstances #-}
module Moves where

import Squares ( Square, pSquare )
import Parsing
  (Alternative((<|>), many), Parser, parse, sat, space )
import Pieces ( PieceType , pPieceType, piece_chars)
import Data.Maybe (isNothing)


-- adts
-- type Move = (InitSquare,EndSquare)
data Move = Move {
    getInitSq :: Square
  , getDestSq :: Square
  , getCrown  :: Maybe PieceType
                 } deriving (Eq)

instance {-# OVERLAPS #-} Show Move where
  show (Move a b c) = show a ++ show b ++ show c

instance {-# OVERLAPS #-} Show (Maybe PieceType) where
  show Nothing = " "
  show (Just a) = show a ++ " "


-- funcs
pLf :: Parser ()
pLf = do
  many (sat (== '\n'))
  return ()

pMoveCoord :: Parser Move
pMoveCoord = do
          space <|> pLf
          sqi <- pSquare
          sqf <- pSquare
          Move sqi sqf <$> pPieceType

readMove :: String -> Maybe Move
readMove str = if null $ parse pMoveCoord str then Nothing
               else Just $ fst $ head $ parse pMoveCoord str

