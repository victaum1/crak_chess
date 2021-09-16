{-# LANGUAGE FlexibleInstances #-}
module Moves where


-- import Squares
-- import Parsing
-- import Pieces
-- import Data.Maybe

import Squares ( Square(..), pSquare )
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
                 } deriving (Eq,Ord)

instance Show Move where
  show (Move a b c) = show a ++ show b ++ show c

-- vars
null_move = Move (Square 0 0) (Square 0 0) Nothing

-- funcs
isNullMove (Move a b _) = a == b 

pLf :: Parser ()
pLf = do
  many (sat (== '\n'))
  return ()

pMoveCoordSimple :: Parser Move
pMoveCoordSimple = do
          sqi <- pSquare
          sqf <- pSquare
          return (Move sqi sqf Nothing)


pMoveCoordCrown :: Parser Move
pMoveCoordCrown = do
          sqi <- pSquare
          sqf <- pSquare
          Move sqi sqf <$> pPieceType


pMoveCoord :: Parser Move
pMoveCoord = do
          space <|> pLf
          pMoveCoordCrown <|> pMoveCoordSimple

readMove :: String -> Maybe Move
readMove str = do
  let m = parse pMoveCoord str
  fst <$> m
