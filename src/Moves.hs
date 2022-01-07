{-# LANGUAGE FlexibleInstances #-}
module Moves where

import Data.Maybe
import Squares
import Parsing
import Pieces
import Utils

-- adts
type Tmove = (Square,Square,Ptype)

-- data Move = Move
--    getInitSq :: Square
--  , getDestSq :: Square
--  , getCrown  :: Ptype
--          } deriving (Eq,Ord)

newtype Move = MkMove{
  fromMove :: Tmove } deriving (Eq,Ord)



instance Show Move where
  show (MkMove (a,b,c)) = showSquare a ++ showSquare b ++ show c

-- vars
null_move = MkMove (MkSquare (0,0), MkSquare (0,0), MkPtype Nothing)

std_w_move = head std_pv
std_b_move = head $ tail std_pv

std_pv = map (myRight . readMove) ["e2e4","e7e5","g1f3"]


-- funcs
getInitSq :: Move -> Square
getInitSq (MkMove (i,_,_)) = i  

getDestSq :: Move -> Square
getDestSq (MkMove (_,f,_)) = f  

getCrown :: Move -> Ptype
getCrown (MkMove (_,_,c)) = c

isNullMove (MkMove (a,b,_)) = a == b 

-- parsing
pMoveCoordS :: GenParser Char st Move
pMoveCoordS = do
  sqi <- pSquare
  sqf <- pSquare
  return (MkMove (sqi,sqf,MkPtype Nothing))

pMoveCoord_ :: GenParser Char st Move
pMoveCoord_ = do
  sqi <- pSquare
  sqf <- pSquare
  mpt <- readPieceType <$> anyChar
  maybe (fail "(not a Crown move)")
    (\a -> return (MkMove (sqi,sqf,a))) mpt

pMoveCoord :: GenParser Char st Move
pMoveCoord = do
  try pMoveCoord_ <|> pMoveCoordS

readMove :: String -> Either ParseError Move
readMove = parse (many space >> pMoveCoord) "(not a move)"
