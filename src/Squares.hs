module Squares (File, Rank, Square(..), intToSquare, showFile, showRank
  , int100ToSquare, pSquare, chrFileList, chrRankList, readRank
  , readCFile, readSquare, files, ranks, squareToInt64,squareToInt100)
  where

import Data.Char (toLower, chr, ord)
import Data.Maybe
import Parsing

type File = Int
type Rank = Int

data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving (Eq, Ord)

instance Show Square where
  show (Square f r) =  showFile f : [showRank r] 


-- Representing Data Structures
chrFileList =  ['a' .. 'h']
chrRankList = ['1' .. '8']

files = [0..7]::[Int]
ranks = files

showFile :: File -> Char
showFile f | or [f<0,f>7] = error "showFile: Out of bounds"
           | otherwise = chr $ 97 + f

showRank :: Rank -> Char
showRank r | or [r<0,r>7] = error "showRank: Out of bounds"
           | otherwise = chr $ 49 + r

toInt c = ord c

-- Parsing Data Structures
readCFile :: Char -> Maybe File
readCFile c | or [c2int <97,c2int>104] = Nothing 
            | otherwise = Just $ c2int - 97  
  where c2int = toInt low_c
        low_c = toLower c

readRank :: Char -> Maybe Rank
readRank c | or [c2int <49,c2int>56] = Nothing 
            | otherwise = Just $ c2int - 49  
  where c2int = toInt c

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
int64ToSquare n | and [n < 64, n >= 0] = Just $ Square a_f a_r
                | otherwise = Nothing
  where a_f = n `mod` 8
        a_r = (n-(n `mod` 8)) `div` 8

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
  where aF = squareFile sq
        aR = squareRank sq

squareToInt100 :: Square -> Int
squareToInt100 sq = int64To100 $ squareToInt64 sq
