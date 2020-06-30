module Squares (File, Rank, Square(..), showFile, showRank
  , pSquare, chrFileList, chrRankList, readRank
  , readCFile, readSquare, files, ranks, tuple2Square, square2Tuple, intToSquare)
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

toInt = ord 



-- Reading Data Structures
readCFile :: Char -> Maybe File
readCFile c | or [c2int <97,c2int>104] = Nothing 
            | otherwise = Just $ c2int - 97  
  where c2int = toInt low_c
        low_c = toLower c

readRank :: Char -> Maybe Rank
readRank c | or [c2int <49,c2int>56] = Nothing 
            | otherwise = Just $ c2int - 49  
  where c2int = toInt c

readSquare' :: Char -> Char -> Maybe Square
readSquare' c1 c2 = if isNothing (mkF c1) || isNothing (mkR c2)
  then Nothing  else Just (Square (fromJust (mkF c1))
  (fromJust (mkR c2)))
  where mkF = readCFile
        mkR = readRank

readSquare :: String -> Maybe Square
readSquare str = if length str < 2  then Nothing else readSquare'
  (head str) (str!!1)

-- Parsing
pFile :: Parser File
pFile = P(\inp -> case inp of 
              [] -> []
              (f:cs) -> [(fromJust (readCFile f), cs
                ) | isJust $ readCFile f])

pRank :: Parser Rank
pRank = P(\inp -> case inp of
             [] -> []
             (r:cs) -> [(fromJust (readRank r), cs
               )| isJust $ readRank r ]
         )

pSquare :: Parser Square
pSquare = do
            f <- pFile
            r <- pRank
            return (Square f r) 


-- Integer tuple encoding
tuple2Square :: (Int,Int) -> Square
tuple2Square (f,r) = Square f r


square2Tuple :: Square -> (Int,Int)
square2Tuple (Square f r) = (f,r)


--- Integers encoding Squares
intToSquare = int64ToSquare

int64ToSquare :: Int -> Maybe Square
int64ToSquare n | and [n < 64, n >= 0] = Just $ Square a_f a_r
                | otherwise = Nothing
  where a_f = n `mod` 8
        a_r = (n-(n `mod` 8)) `div` 8

