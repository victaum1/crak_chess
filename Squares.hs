module Squares (File(..), Rank(..), Square(..)) where

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

chr2File' = zip fileList chrFileList

chr2Rank' = zip rankList chrRankList

showFile :: File -> Char
showFile f = fromJust (lookup f chr2File')

showRank :: Rank -> Char
showRank r = fromJust (lookup r chr2Rank')

instance Show Square where
  show (Square a b) =  [showFile a] ++ [showRank b] 

