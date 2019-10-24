module Squares (File(..), Rank(..), Square(..), int64ToSquare
  , int120ToSquare, pSquare, chrFileList, chrRankList, readRank, readCFile
  , readSquare)
  where

import Data.Char (toUpper)
import Data.Maybe
import Parsing

data File = A_F | B_F | C_F | D_F | E_F | F_F | G_F | H_F
  deriving (Show,Eq,Ord)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show,Eq,Ord)

data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving Eq

chrFileList = "ABCDEFGH"
chrRankList = "12345678"

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
int120ToSquare n | or [n `mod` 10 == 0, n `mod` 10 == 9]  = Nothing
                 | and [n > 98, n < 21] = Nothing 
                 | otherwise = int64ToSquare (int120To64 n)

fileList = [A_F,B_F,C_F,D_F,E_F,F_F,G_F,H_F]
rankList = [R1,R2,R3,R4,R5,R6,R7,R8]

chr2File = zip chrFileList fileList
chr2File' = zip fileList chrFileList

chr2Rank = zip chrRankList rankList
chr2Rank' = zip rankList chrRankList

showFile :: File -> Char
showFile f = fromJust (lookup f chr2File')

showRank :: Rank -> Char
showRank r = fromJust (lookup r chr2Rank')

instance Show Square where
  show (Square a b) =  showFile a : [showRank b] 

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
