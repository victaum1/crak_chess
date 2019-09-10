module Board (Pos(..), Board) where

import Pieces
import Squares
import Data.Maybe

initBoardStr = unlines [
                         "rnbqkbnr","pppppppp","........","........"
                       , "........","........","PPPPPPPP","RNBQKBNR"
                       ]

data Pos = Pos {getSquare :: Square, getPiece :: Piece}

instance Show Pos where
  show (Pos a b) = "(" ++ show a ++ "," ++ show b ++ ")"

type Board = [Pos]

makePos :: [Maybe Piece] -> Int -> [Pos]
makePos [] _ = []
makePos (a:as) n | isJust a = (Pos (fromJust $ intToSquare n) (fromJust a)): 
                    nexT
                 | otherwise = nexT
                   where nexT = makePos as (n+1)

readBoard :: String -> Board
readBoard str = toPos $ reverse $ readBoard' str
  where toPos (p:ps) = makePos (concat (p:ps)) 0

readBoard' :: String -> [[Maybe Piece]]
readBoard' = (map.map) readPiece . lines

showBoard' :: Board -> [String]
showBoard' ps = (map.map) showP $ reverse $ splitb lps
  where lps = [checkSquare ps (toSq n) | n <- [0..63]]
        toSq n = fromJust $ intToSquare n
        splitb [] = []
        splitb ps = (take 8 ps):(splitb $ drop 8 ps)
        showP p | isJust p = showPiece $ fromJust p
                | otherwise = '.'

showBoard :: Board -> String
showBoard = unlines . showBoard'

checkSquare :: Board -> Square -> Maybe Piece
checkSquare [] _ = Nothing
checkSquare (p:ps) sq | (getSquare p) == sq = Just (getPiece p)
                      | otherwise = checkSquare ps sq

