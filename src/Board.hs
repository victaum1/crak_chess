module Board (Board, Pos, init_board_fen, init_board, fen2Board, pFenBoard,
  board2FEN, pBoard, showBoard, readBoard) where

import Data.Char
import Data.Maybe
import Parsing
import Pieces
import Squares

-- vars
init_board_str = unlines [
                         "rnbqkbnr","pppppppp","........","........"
                       , "........","........","PPPPPPPP","RNBQKBNR"
                       ]
init_board_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR "
init_board = fen2Board init_board_fen


-- adts
type Pos = (Square,Piece)
type Board = [Pos]
type BoardList = [Maybe Piece]


-- funcs
makePos :: [Maybe Piece] -> Int -> Board
makePos [] _ = []
makePos (a:as) n | isJust a = (fromJust $ intToSquare n,fromJust a) :
                    nexT
                 | otherwise = nexT
                   where nexT = makePos as (n+1)


readBoard :: String -> Board
readBoard str = if isNull then []
                else (toPos . reverse) (fromJust rB)
  where toPos ps = makePos (concat ps) 0
        isNull = isNothing rB
        rB = readBoard' str


inPiece r = toUpper r `elem` piece_fen_list
filterBoard :: String -> String
filterBoard [] = []
filterBoard [r]    | inPiece r = [r]
                   | otherwise = []
filterBoard (r:cs) | inPiece r = if null (filterBoard cs)
                                   then []
                                   else r:filterBoard cs
                   | otherwise = []


readBoard' :: String -> Maybe [[Maybe Piece]]
readBoard' str = if null (filterBoard $ concat $ lines str) then Nothing
                   else Just (readBoard_ (lines str))


readBoard_ :: [String] -> [[Maybe Piece]]
readBoard_ = (map . map) readCPiece


showBoard' :: Board -> [String]
showBoard' ps = (map.map) showP $ reverse $ splitb lps
  where lps = [checkSquare ps (toSq n) | n <- [0..63]]
        toSq n = fromJust $ intToSquare n
        splitb [] = []
        splitb xs = take 8 xs : splitb (drop 8 xs)
        showP p | isJust p = showPiece $ fromJust p
                | otherwise = '.'


showBoard :: Board -> String
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
pBoard = P (\inp -> if not $ null $ res inp then [(res inp, drop 72 inp)]
                    else []
              )
         where res x = if length x >= 72 then readBoard $ take 72 x
                       else []

-- bL2Board :: Int -> BoardList -> Board
-- bL2Board _ [] = []
-- bL2Board _ [Nothing] = []
-- bL2Board n [Just p] = [(fromJust $ intToSquare n,p)]
-- bL2Board n (Just p:cs) = (fromJust $ intToSquare n,p)
--   :bL2Board (n+1) cs
-- bL2Board n (Nothing:cs) = bL2Board (n+1) cs

pFenBoard :: Parser Board
pFenBoard = P(\str -> case str of
                 [] -> []
                 _  -> if isNull $ f2B str then []
                   else [(f2B str, unwords $ tail $ words str)]
             )
            where f2B    = fen2Board
                  isNull = null

