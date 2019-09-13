module Board (Pos(..), Board, initBoardFEN, fen2Board,
  board2FEN, pBoard) where

import Pieces
import Squares
import Data.Maybe
import Data.Char
import Parsing

initBoardStr = unlines [
                         "rnbqkbnr","pppppppp","........","........"
                       , "........","........","PPPPPPPP","RNBQKBNR"
                       ]

initBoardFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

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

subs :: Char -> Char -> String -> String
subs ic oc (c:cs) | c == ic = oc:(subs ic oc cs)
                  | otherwise = c:(subs ic oc cs)
subs _  _ [] = []

showFENline :: String -> String
showFENline = (showFENline' . subs '/' '\n') 

showFENline' :: String -> String
showFENline' [] = []
showFENline' (c:cs) | isDigit c = (take (digitToInt c) $ cycle ['.']) ++
  (showFENline' cs)
                   | otherwise = c:(showFENline' cs)

countDots :: String -> Int -> Int
countDots (c:cs) n | c == '.' = countDots cs (n+1)
                   | otherwise = n
countDots [] n = n 

packFENline :: String -> String
packFENline [] = []
packFENline (c:cs) | c == '.' = (intToDigit (countDots (c:cs) 0)):
  packFENline (drop (countDots (c:cs) 0) (c:cs))
                   | otherwise = c:(packFENline cs)

fen2Board :: String -> Board
fen2Board str = readBoard $ showFENline $ head $ words str

board2FEN :: Board -> String
board2FEN bd = init $ packFENline $ subs '\n' '/' $ showBoard bd

pBoard :: Parser Board
pBoard = P (\inp -> case inp of
  [] -> [] 
  _ -> [(fen2Board inp, unwords $ tail $ words inp)])

