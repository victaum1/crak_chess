module Board (Board, Pos(..), initBoardFEN, fen2Board,
  board2FEN, pBoard, showBoard, readBoard) where

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

type Pos = (Square,Piece)

type Board = [Pos] 


makePos :: [Maybe Piece] -> Int -> Board
makePos [] _ = []
makePos (a:as) n | isJust a = ((fromJust $ intToSquare n),(fromJust a)) :
                    nexT
                 | otherwise = nexT
                   where nexT = makePos as (n+1)


readBoard :: String -> Board
readBoard str = if isNull then []
                else (toPos .reverse) (fromJust rB) 
  where toPos ps = makePos (concat ps) 0
        isNull = isNothing rB 
        rB = readBoard' str 


readBoard' :: String -> Maybe [[Maybe Piece]]
readBoard' = (mapM . mapM) readCPiece . lines


showBoard' :: Board -> [String]
showBoard' ps = (map.map) showP $ reverse $ splitb lps
  where lps = [checkSquare ps (toSq n) | n <- [0..63]]
        toSq n = fromJust $ intToSquare n
        splitb [] = []
        splitb xs = take 8 xs : splitb (drop 8 xs)
        showP p | isJust p = showPiece $ fromJust p
                | otherwise = '.'


showBoard :: Board-> String
showBoard = unlines . showBoard'


checkSquare :: Board -> Square -> Maybe Piece
checkSquare bd sq = lookup sq bd


subs :: Char -> Char -> String -> String
subs _  _ [] = []
subs ic oc (c:cs) | c == ic = oc : subs ic oc cs
                  | otherwise = c : subs ic oc cs


showFENline :: String -> String
showFENline = showFENline' . subs '/' '\n' 


showFENline' :: String -> String
showFENline' [] = []
showFENline' (c:cs) | isDigit c = replicate (digitToInt c) '.' ++
  showFENline' cs
                    | otherwise = c: showFENline' cs


countDots :: String -> Int -> Int
countDots [] n = n
countDots (c:cs) n | c == '.' = countDots cs (n+1)
                   | otherwise = n


packFENline :: String -> String
packFENline [] = []
packFENline (c:cs) | c == '.' = intToDigit (countDots (c:cs) 0) :
  packFENline (drop (countDots (c:cs) 0) (c:cs))
                   | otherwise = c : packFENline cs


fen2Board :: String -> Board
fen2Board str = readBoard $ showFENline $ head $ words str


board2FEN :: Board -> String
board2FEN bd = init $ packFENline $ subs '\n' '/' $ showBoard bd


pBoard :: Parser Board
pBoard = P (\inp -> case inp of
  [] -> [] 
  _ -> [(fen2Board inp, unwords $ tail $ words inp
         )| not $ null $ fen2Board inp])

