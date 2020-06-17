{-# LANGUAGE TemplateHaskell #-}
import Squares
import Test.QuickCheck.All
import Test.QuickCheck 
import Data.Char
import Data.Maybe (fromJust, isNothing, isJust)
import qualified System.Exit as Exit

strSquareList = [a:b:"" | a <- chrFileList, b <- chrRankList]
squares = [Square f r | f <- files, r <- ranks]

newtype SafeRank = SRank {getSafeRank::Char} deriving Show

instance Arbitrary SafeRank where
  arbitrary = SRank <$> (elements chrRankList) 

instance Arbitrary Square where
  arbitrary = elements squares

prop_successReadFile :: Char -> Property
prop_successReadFile c = isJust (readCFile c) ==> elem (toLower c) chrFileList

prop_failureReadFile :: Char -> Property
prop_failureReadFile c = isNothing (readCFile c) ==> not (elem (toLower c) chrFileList)

prop_ReadFileIdemPot :: Char -> Bool 
prop_ReadFileIdemPot c = if elem (toLower c) chrFileList
  then showFile (fromJust $ readCFile c) == (toLower c)
  else isNothing (readCFile c)

prop_successReadRank :: SafeRank -> Bool
prop_successReadRank (SRank c) = isJust (readRank c)

prop_failureReadRank :: Char -> Property
prop_failureReadRank c = isNothing (readRank c) ==> not (elem (toLower c) chrRankList)

prop_ReadRankIdemPot :: Char -> Bool 
prop_ReadRankIdemPot c = if elem (toLower c) chrRankList
  then showRank (fromJust $ readRank c) == (toLower c)
  else isNothing (readRank c)

prop_showSquare :: Square -> Bool
prop_showSquare sq = elem (show sq) strSquareList

prop_readSquare :: String -> Bool
prop_readSquare str = if  isNothing (readSquare str)  
  then not (is_str str)  else is_sq str
  where is_sq s = elem (fromJust (readSquare (map toUpper s)))
          squares
        is_str s = elem (take 2 (map toUpper s)) strSquareList

-- prop_int_to_square :: Int -> Bool
-- prop_int_to_square n | or [n < 0, n > 63] = chkNo
--                      | otherwise = chkSq 
--   where chkNo = isNothing $ intToSquare n
--         chkSq = elem (fromJust $ intToSquare n) squares
-- 
-- prop_int100_to_square :: Int -> Bool
-- prop_int100_to_square n | or [n < 11 , n > 89] = chkNo
--                         | or [n `mod` 10 == 0, n `mod` 10 == 9] = chkNo 
--                         | otherwise = chkSq 
--   where chkNo = isNothing $ int100ToSquare n
--         chkSq = elem (fromJust $ int100ToSquare n) squares
-- 
-- prop_square_to_int64 :: Square -> Bool
-- prop_square_to_int64 sq | or [chkInt >= 0, chkInt < 64] = True
--                         | otherwise = False
--   where chkInt = squareToInt64 sq
-- 
-- prop_square_to_int100 :: Square -> Bool
-- prop_square_to_int100 sq | and [or [chkInt >= 11, chkInt < 89],
--   or [chkInt `mod` 10 /= 0, chkInt `mod` 10 /= 9]] = True
--                          | otherwise = False
--   where chkInt = squareToInt100 sq

return []
runTests :: IO Bool
runTests = $forAllProperties
  (quickCheckWithResult stdArgs {maxSuccess = 200})

main :: IO ()
main = do
         x <- runTests
         if x then
           do
             return ()
         else
           do
             Exit.exitFailure
