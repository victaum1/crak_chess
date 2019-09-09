module Squares (File(..), Rank(..), Square(..)) where

data File = A_F | B_F | C_F | D_F | E_F | F_F | G_F | H_F
  deriving (Show,Eq,Ord)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show,Eq,Ord)

data Square = Square {
                         squareFile :: File
                       , squareRank :: Rank
                      } deriving Eq

instance Show Square where
  show (Square a b) =  "("++ show a ++","++ show b ++ ")"

