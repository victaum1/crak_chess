{-# LANGUAGE FlexibleInstances #-}
module Moves where


import Squares
import Parsing
import Pieces
import Data.Maybe

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
null_move = MkMove (MkSquare (0,0), MkSquare (0,0), Ptype Nothing)

-- funcs


getInitSq :: Move -> Square
getInitSq (MkMove (i,_,_)) = i  

getDestSq :: Move -> Square
getDestSq (MkMove (_,f,_)) = f  

getCrown :: Move -> Ptype
getCrown (MkMove (_,_,c)) = c

isNullMove (MkMove (a,b,_)) = a == b 

pLf :: Parser ()
pLf = do
  many (sat (== '\n'))
  return ()

pMoveCoordSimple :: Parser Move
pMoveCoordSimple = do
          sqi <- pSquare
          sqf <- pSquare
          return (MkMove (sqi, sqf, Ptype Nothing))

pMoveCoordCrown :: Parser Move
pMoveCoordCrown = do
          sqi <- pSquare
          sqf <- pSquare
          pt <- pPieceType
          return (MkMove (sqi, sqf, pt))


pMoveCoord :: Parser Move
pMoveCoord = do
          space <|> pLf
          pMoveCoordCrown <|> pMoveCoordSimple

readMove :: String -> Maybe Move
readMove str = do
  let m = parse pMoveCoord str
  fst <$> m
