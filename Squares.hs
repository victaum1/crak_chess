module Squares (File(..), Rank(..), Square(..)) where

import Data.Char (toUpper)
import Data.Maybe

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
  show (Square a b) =  [showFile a] ++ [showRank b] 

readCFile :: Char -> Maybe File
readCFile c = lookup (toUpper c) chr2File 

readRank :: Char -> Maybe Rank
readRank c = lookup (toUpper c) chr2Rank 

makeSquare :: Char -> Char -> Maybe Square
makeSquare c1 c2 = if (isNothing (mkF c1) || isNothing (mkR c2))
  then Nothing  else Just (Square (fromJust (mkF c1))
  (fromJust (mkR c2)))
  where mkF = readCFile
        mkR = readRank

readSquare :: String -> Maybe Square
readSquare str = if (length str < 2)  then Nothing else makeSquare  (str!!0)
 (str!!1)

