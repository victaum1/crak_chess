module Moves where

import Squares
import Parsing

-- adts
-- type Move = (InitSquare,EndSquare)
data Move = Move {
    getInitSq :: Square
  , getDestSq :: Square
                 } deriving (Eq)

instance Show Move where
  show (Move a b) = show a ++ show b


-- funcs
pLf :: Parser ()
pLf = do
  _ <- many (sat (== '\n'))
  return ()


pMoveCoord :: Parser Move
pMoveCoord = do
          space <|> pLf
          sqi <- pSquare
          Move sqi <$> pSquare


readMove :: String -> Maybe Move
readMove str = if null $ parse pMoveCoord str then Nothing
                 else Just $ fst $ head $ parse pMoveCoord str

