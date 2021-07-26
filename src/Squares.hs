{-# LANGUAGE LambdaCase #-}
module Squares (File, Rank,SquareTuple, Square(..), showFile, showRank
  , pSquare, readRank, readCFile, readSquare, files, ranks, tuple2Square
  , square2Tuple, intToSquare, file_chrs, rank_chrs)
  where

import           Data.Char  (chr, ord, toLower)
import           Data.Maybe
import           Parsing

-- vars
file_chrs = ['a' .. 'h']
rank_chrs = ['1' .. '8']
files = [0..7]::[Int]
ranks = files


--adts
type File = Int
type Rank = Int
type SquareTuple = (Int,Int)


data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving (Eq, Ord)

instance Show Square where
  show (Square f r) =  showFile f : [showRank r]


-- funcs
showFile :: File -> Char
showFile f | (f<0) || (f>7) = error "showFile: Out of bounds"
           | otherwise = chr $ 97 + f


showRank :: Rank -> Char
showRank r | (r<0) || (r>7) = error "showRank: Out of bounds"
           | otherwise = chr $ 49 + r

toInt = ord

-- Reading Data Structures / Parsing
readCFile :: Char -> Maybe File
readCFile c | (c2int <97) || (c2int>104) = Nothing
            | otherwise = Just $ c2int - 97
  where c2int = toInt low_c
        low_c = toLower c

readRank :: Char -> Maybe Rank
readRank c | (c2int <49) || (c2int>56) = Nothing
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


pFile :: Parser File
pFile = P(\case
    [] -> []
    (f:cs) -> [(fromJust (readCFile f), cs
         ) | isJust $ readCFile f])

pRank :: Parser Rank
pRank = P(\case
             [] -> []
             (r:cs) -> [(fromJust (readRank r), cs
               )| isJust $ readRank r ]
         )

pSquare :: Parser Square
pSquare = do
            f <- pFile
            Square f <$> pRank

-- Integer tuple encoding
tuple2Square :: SquareTuple -> Square
tuple2Square (f,r) = Square f r


square2Tuple :: Square -> SquareTuple
square2Tuple (Square f r) = (f,r)

--- Integers encoding Squares
intToSquare = int64ToSquare

int64ToSquare :: Int -> Maybe Square
int64ToSquare n | (n < 64) && (n >= 0) = Just $ Square a_f a_r
                | otherwise = Nothing
  where a_f = n `mod` 8
        a_r = (n-(n `mod` 8)) `div` 8
