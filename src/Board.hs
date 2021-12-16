module Board where

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

empty_board = MkBoard Map.empty

-- adts
type Pos = (SquareTuple, Tpiece)
type Tboard = Map SquareTuple Tpiece
-- type PieceList_ = Map Tpiece [SquareTuple]
type PieceList = Map Piece [Square]
type PosList = [Pos]

newtype Board = MkBoard { fromBoard::Tboard} deriving (Eq)

instance Show Board where
  show b = showBoard b

-- funcs
makePosList :: [Maybe Piece] -> Int -> PosList
makePosList [] _ = []
makePosList (a:as) n | isJust a = (square2Tuple (fromJust (intToSquare n))
                                  , fromPiece (fromJust a)) : nexT
                     | otherwise = nexT
                   where nexT = makePosList as (n+1)


readBoard :: String -> Board
readBoard = MkBoard . Map.fromList . readBoardList


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
showBoard = showBoard_ . Map.toAscList . fromBoard

showBoard_ = unlines . showBoardList

showBoardList :: PosList -> [String]
showBoardList ps = (map.map) showP (reverse $ splitb lps)
  where lps = [Map.lookup  (square2Tuple (toSq n)) (Map.fromList ps) | n <- [0..63]]
        toSq n = fromJust $ intToSquare n
        splitb [] = []
        splitb xs = take 8 xs : splitb (drop 8 xs)
        showP p | isJust p = showPiece $ MkPiece (fromJust p)
                | otherwise = '.'


checkSquare :: Square -> Board -> Maybe Piece
checkSquare (MkSquare sq) (MkBoard bd) = MkPiece <$> Map.lookup sq bd


checkPiece :: Piece -> PieceList -> Maybe [Square]
checkPiece = Map.lookup

checkPiece_ :: Piece -> Board -> [Square]
checkPiece_  p b = fromMaybe [] $ checkPiece p (makePieceList b)


whereIsPiece :: Piece -> Board -> [Square]
whereIsPiece (MkPiece p) (MkBoard b) = map MkSquare  <$> map fst $
  Map.toList $ Map.filter (p ==) b
 

whereIsKing s b = head $ whereIsPiece (MkPiece (s,King)) b


makePieceList :: Board -> PieceList
makePieceList bd = Map.fromList $ zip all_pieces $ map
                                     (`whereIsPiece` bd)
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
fen2Board = MkBoard . Map.fromList . fen2Board_

fen2Board_ :: String -> PosList
fen2Board_ = readBoardList . showFENline . head . words


board2FEN :: Board -> String
board2FEN = board2FEN_ . Map.toAscList .fromBoard

board2FEN_ :: PosList -> String
board2FEN_ = init . packFENline . subs '\n' '/' . showBoard_

pBoard :: GenParser Char st Board
pBoard = do
  inp <- many anyChar
  let ib = res inp
  if null ib then fail "(not a board)" else do
    let r = MkBoard (Map.fromList ib)
    let s = drop (length $ showBoard r) inp
    setInput s
    return r
  where res x = if length x >= 72 then readBoardList $ take 72 x
        else []

pFenBoard :: GenParser Char st Board
pFenBoard = do
  inp <- many anyChar
  let ib = f2B_ inp
  if null ib then fail "(not a FEN)" else do
    let r = MkBoard $ Map.fromList ib
    let s = drop (length $ board2FEN r) inp
    setInput s
    return r
  where f2B_ = fen2Board_

