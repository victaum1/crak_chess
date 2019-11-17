module Squares (File, Rank, Square(..), intToSquare, showFile, showRank
  , int100ToSquare, pSquare, chrFileList, chrRankList, readRank
  , readCFile, readSquare, files, ranks, squareToInt64,squareToInt100)
  where

import Data.Char (toUpper, toLower)
import Data.Maybe
import Parsing

-- Data Structures
-- data File = __F | A_F | B_F | C_F | D_F | E_F | F_F | G_F | H_F | I_F
--   deriving (Show,Eq,Ord,Enum)
-- 
-- data Rank = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
--   deriving (Show,Eq,Ord,Enum)

type File = Int
type Rank = Int

data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving Eq


-- Representing Data Structures
chrFileList = "abcdefgh"
chrRankList = "12345678"

files = [0..7]
ranks = files


chr2File = zip chrFileList files
chr2File' = zip files chrFileList

chr2Rank = zip chrRankList ranks
chr2Rank' = zip ranks chrRankList

showFile :: File -> Char
showFile f = fromJust (lookup f chr2File')

showRank :: Rank -> Char
showRank r = fromJust (lookup r chr2Rank')

chrFileToInt = zip chrFileList files
chrFileToInt' = zip files chrFileList

instance Show Square where
  show (Square a b) =  showFile a : [showRank b] 

-- Parsing Data Structures
readCFile :: Char -> Maybe File
readCFile c = lookup (toLower c) chr2File 

readRank :: Char -> Maybe Rank
readRank c = lookup c chr2Rank 

makeSquare :: Char -> Char -> Maybe Square
makeSquare c1 c2 = if isNothing (mkF c1) || isNothing (mkR c2)
  then Nothing  else Just (Square (fromJust (mkF c1))
  (fromJust (mkR c2)))
  where mkF = readCFile
        mkR = readRank

readSquare :: String -> Maybe Square
readSquare str = if length str < 2  then Nothing else makeSquare
  (head str) (str!!1)

pFile :: Parser File
pFile = P(\inp -> case inp of 
              [] -> []
              (f:cs) -> if isNothing (readCFile f) then
                             []
                        else [(fromJust (readCFile f),cs)])

pRank :: Parser Rank
pRank = P(\inp -> case inp of
             [] -> []
             (r:cs) -> if isNothing(readRank r) then []
                       else [(fromJust (readRank r),cs)])

pSquare :: Parser Square
pSquare = do
            f <- pFile
            r <- pRank
            return (Square f r) 

-- Integers encoding Squares
intToSquare = int64ToSquare

int64ToSquare :: Int -> Maybe Square
int64ToSquare n | n < 64 && n >= 0 = makeSquare a_file a_rank
              | otherwise = Nothing
  where a_file = chrFileList!!(n `mod` 8)
        a_rank = chrRankList!!((n-(n `mod` 8)) `div` 8)

int100To64 :: Int -> Int
int100To64 n = toDec * 8 + toDig - 1 
  where toDig = n `mod` 10
        toDec = (n - toDig - 10) `div` 10

int100ToSquare :: Int -> Maybe Square
int100ToSquare n | or [n > 88, n < 11] = Nothing 
                 | or [n `mod` 10 == 0, n `mod` 10 == 9]  = Nothing
                 | otherwise = int64ToSquare (int100To64 n)

int64To100 :: Int -> Int
int64To100 n = toRow*10 + toFile + 11
  where toFile = n `mod` 8
        toRow = (n - toFile) `div` 8

squareToInt64 :: Square -> Int
squareToInt64 sq = aR*8 + aF
  where aF = fromJust $ lookup (showFile $ squareFile sq) chrFileToInt
        aR = (read [showRank $ squareRank sq]) - 1

squareToInt100 :: Square -> Int
squareToInt100 sq = int64To100 $ squareToInt64 sq
