module Squares
  where

-- import           Data.Char
-- import           Data.Maybe
-- import           Parsing

import Data.Char ( ord, chr, toLower )
import Data.Maybe ( fromJust, isNothing )
import Parsing

-- vars
file_chrs = ['a' .. 'h']
rank_chrs = ['1' .. '8']
files = [0..7]::[Int]
ranks = files
all_squares = zipWith (curry MkSquare) files ranks


--adts
type File = Int
type Rank = Int
type SquareTuple = (Int,Int)


newtype Square = MkSquare {
  fromSquare :: SquareTuple } deriving (Eq,Ord)

instance Show Square where
 show = showSquare


-- funcs
squareRank :: Square -> Rank
squareRank (MkSquare (f,r)) = r

squareFile :: Square -> File
squareFile (MkSquare (f,r)) = f


showFile :: File -> Char
showFile f | (f<0) || (f>7) = error "showFile: Out of bounds"
           | otherwise = chr $ 97 + f


showRank :: Rank -> Char
showRank r | (r<0) || (r>7) = error "showRank: Out of bounds"
           | otherwise = chr $ 49 + r

showSquare :: Square -> String
showSquare (MkSquare a) = showFile (fst a) : [showRank (snd a)]

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
  then Nothing  else Just (MkSquare (fromJust (mkF c1),
  fromJust (mkR c2)))
  where mkF = readCFile
        mkR = readRank

readSquare :: String -> Maybe Square
readSquare str = if length str < 2  then Nothing else readSquare'
  (head str) (str!!1)

-- parsing 
pFile :: GenParser Char st File
pFile = do
  mf <- readCFile <$> anyChar
  maybe (fail "(not a file)") return mf

pRank :: GenParser Char st Rank
pRank = do
  mf <- readRank <$> anyChar
  maybe (fail "(not a rank)") return mf

pSquare :: GenParser Char st Square
pSquare = do
  f <- pFile
  r <- pRank
  return (MkSquare (f,r))

-- Integer tuple encoding
tuple2Square :: SquareTuple -> Square
tuple2Square (f,r) = MkSquare (f,r)


square2Tuple :: Square -> SquareTuple
square2Tuple = fromSquare

--- Integers encoding Squares
intToSquare = int64ToSquare

int64ToSquare :: Int -> Maybe Square
int64ToSquare n | (n < 64) && (n >= 0) = Just $ MkSquare (a_f,a_r)
                | otherwise = Nothing
  where a_f = n `mod` 8
        a_r = (n-(n `mod` 8)) `div` 8
