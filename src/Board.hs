module Board (Board, Pos, init_board_str, init_board_fen, init_board,
  fen2Board, pFenBoard,board2FEN, pBoard, showBoard, readBoard, checkSquare, whereIsPiece, makePieceList) where

import Data.Char
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parsing
import Pieces
import Squares

-- vars / const
init_board_str = unlines [
  "rnbqkbnr","pppppppp","........","........"
  , "........","........","PPPPPPPP","RNBQKBNR"
  ]

all_fen_chars = "rnbqkpRNBQKP/12345678"

init_board_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR "
init_board = fen2Board init_board_fen

-- adts
type Pos = (Square,Piece)
type Board = Map Square Piece
type PieceList = Map Piece [Square]
type PosList = [Pos]

-- funcs
makePosList :: [Maybe Piece] -> Int -> PosList
makePosList [] _ = []
makePosList (a:as) n | isJust a = (fromJust $ intToSquare n,fromJust a) : nexT
                     | otherwise = nexT
                   where nexT = makePosList as (n+1)

readBoard :: String -> Board
readBoard = Map.fromList . readBoardList

readBoardList :: String -> PosList
readBoardList str | isNull = []
                  | otherwise = (toPos . reverse) (fromJust rB)
                  where toPos ps = makePosList (concat ps) 0
                        isNull = isNothing rB
                        rB = readBoardList' str

readBoardList' :: String -> Maybe [[Maybe Piece]]
readBoardList' str | null (filterBoard $ concat $ lines str) =
                     Nothing
                   | otherwise =Just $ readBd $ lines str
                   where readBd = (map . map) readCPiece

filterBoard :: String -> String
filterBoard [] = []
filterBoard [r]    | inPiece r = [r]
                   | otherwise = []
filterBoard (r:cs) | inPiece r = if null (filterBoard cs)
                                   then []
                                   else r:filterBoard cs
                   | otherwise = []

inPiece r = r `elem` board_piece_chars

showBoard :: Board -> String
showBoard = showBoard_ . Map.toAscList

showBoard_ = unlines . showBoardList

showBoardList :: PosList -> [String]
showBoardList ps = (map.map) showP $ reverse $ splitb lps
  where lps = [Map.lookup  (toSq n) (Map.fromList ps) | n <- [0..63]]
        toSq n = fromJust $ intToSquare n
        splitb [] = []
        splitb xs = take 8 xs : splitb (drop 8 xs)
        showP p | isJust p = showPiece $ fromJust p
                | otherwise = '.'


checkSquare :: Square -> Board -> Maybe Piece
checkSquare = Map.lookup

checkPiece :: Piece -> PieceList -> Maybe [Square]
checkPiece = Map.lookup

whereIsPiece :: Piece -> Board -> [Square]
whereIsPiece p b = map fst $ Map.toList $ Map.filter (p ==) b


makePieceList :: Board -> PieceList
makePieceList bd = Map.fromList $ zip all_pieces $ map (`whereIsPiece` bd)
  all_pieces


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
fen2Board = Map.fromList . fen2Board_

fen2Board_ :: String -> PosList
fen2Board_ = readBoardList . showFENline . head . words


board2FEN :: Board -> String
board2FEN = board2FEN_ . Map.toAscList

board2FEN_ :: PosList -> String
board2FEN_ = init . packFENline . subs '\n' '/' . showBoard_


pBoard :: Parser Board
pBoard = P (\inp -> case res inp of
               [] -> Nothing
               _ -> Just (Map.fromList $ res inp,drop 72 inp))
         where res x = if length x >= 72 then readBoardList $ take 72 x
                       else []

pFenBoard :: Parser Board
pFenBoard = P(\str -> case str of
                 [] -> Nothing
                 _  -> if null (f2B_ str) then Nothing else
                   Just (f2B str,unwords $ tail $ words str))
            where f2B    = fen2Board
                  f2B_   = fen2Board_
