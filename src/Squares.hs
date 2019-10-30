module Squares (File(..), Rank(..), Square(..), intToSquare
  , int120ToSquare, pSquare, chrFileList, chrRankList, readRank, readCFile
  , readSquare, files, ranks)
  where

import Data.Char (toUpper)
import Data.Maybe
import Parsing

-- Data Structures
data File = A_F | B_F | C_F | D_F | E_F | F_F | G_F | H_F
  deriving (Show,Eq,Ord)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show,Eq,Ord)

data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving Eq


-- Representing Data Structures
chrFileList = "ABCDEFGH"
chrRankList = "12345678"

files = [A_F,B_F,C_F,D_F,E_F,F_F,G_F,H_F]
ranks = [R1,R2,R3,R4,R5,R6,R7,R8]

chr2File = zip chrFileList files
chr2File' = zip files chrFileList

chr2Rank = zip chrRankList ranks
chr2Rank' = zip ranks chrRankList

showFile :: File -> Char
showFile f = fromJust (lookup f chr2File')

showRank :: Rank -> Char
showRank r = fromJust (lookup r chr2Rank')

chrFileToInt = zip chrFileList [0..7]
chrFileToInt' = zip [0..7] chrFileList

instance Show Square where
  show (Square a b) =  showFile a : [showRank b] 

-- Parsing Data Structures
readCFile :: Char -> Maybe File
readCFile c = lookup (toUpper c) chr2File 

readRank :: Char -> Maybe Rank
readRank c = lookup (toUpper c) chr2Rank 

makeSquare :: Char -> Char -> Maybe Square
makeSquare c1 c2 = if isNothing (mkF c1) || isNothing (mkR c2)
  then Nothing  else Just (Square (fromJust (mkF c1))
  (fromJust (mkR c2)))
  where mkF = readCFile
        mkR = readRank

readSquare :: String -> Maybe Square
readSquare str = if length str < 2  then Nothing else makeSquare  (head str)
 (str!!1)

pFile :: Parser (Maybe File)
pFile = P(\inp -> case inp of 
              [] -> []
              (f:cs) -> if isNothing (readCFile f) then
                             []
                        else [(readCFile f,cs)])

pRank :: Parser (Maybe Rank)
pRank = P(\inp -> case inp of
             [] -> []
             (r:cs) -> if isNothing(readRank r) then []
                       else [(readRank r,cs)])

pNoSq :: Parser (Maybe Char)
pNoSq = P(\inp -> case inp of
             [] -> []
             (r:cs) -> if r == '-' then [(Nothing,cs)]
                       else [(Just r, r:cs)])

pSquare :: Parser (Maybe Square)
pSquare = do
            nsq <- pNoSq
            if isNothing nsq then
              return Nothing
            else
              do
                f <- pFile
                r <- pRank
                return (Just $ Square (fromJust f) (fromJust r)) 

-- Integers encoding Squares
intToSquare = int64ToSquare

int64ToSquare :: Int -> Maybe Square
int64ToSquare n | n < 64 && n >= 0 = makeSquare a_file a_rank
              | otherwise = Nothing
  where a_file = chrFileList!!(n `mod` 8)
        a_rank = chrRankList!!((n-(n `mod` 8)) `div` 8)

int120To64 :: Int -> Int
int120To64 n = toDec * 8 + toDig - 1 
  where toDig = n `mod` 10
        toDec = (n - toDig - 20) `div` 10

int120ToSquare :: Int -> Maybe Square
int120ToSquare n | or [n > 98, n < 21] = Nothing 
                 | or [n `mod` 10 == 0, n `mod` 10 == 9]  = Nothing
                 | otherwise = int64ToSquare (int120To64 n)

int64To120 :: Int -> Int
int64To120 n = toRow*10 + toFile + 21
  where toFile = n `mod` 8
        toRow = (n - toFile) `div` 8

squareToInt64 :: Square -> Int
squareToInt64 sq = aR*8 + aF
  where aF = fromJust $ lookup (showFile $ squareFile sq) chrFileToInt
        aR = (read [showRank $ squareRank sq]) - 1

squareToInt120 :: Square -> Int
squareToInt120 sq = int64To120 $ squareToInt64 sq
