module Moves where

import Data.Char
import Squares
import Pieces
import Parsing

-- adts
type InitSquare = Square
type EndSquare = Square

-- type Move = (InitSquare,EndSquare)
data Move = Move {
  getInitSq :: Square
  , getDestSq :: Square
                 } deriving (Eq)

instance Show Move where
  show (Move a b) = show a ++ show b


-- funcs
pLf :: Parser ()
pLf = do many (sat (== '\n'))
         return ()


pMoveCoord :: Parser Move
pMoveCoord = do
          space <|> pLf
          sqi <- pSquare
          sqf <- pSquare
          return (Move sqi sqf)


readMove :: String -> Maybe Move
readMove str = if null $ parse pMoveCoord str then Nothing
                 else Just $ fst $ head $ parse pMoveCoord str

