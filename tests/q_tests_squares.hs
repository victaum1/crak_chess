{-# LANGUAGE TemplateHaskell #-}
import Squares
import Test.QuickCheck.All
import Test.QuickCheck 
import Data.Char
import Data.Maybe (fromJust, isNothing)
import qualified System.Exit as Exit

strSquareList = [a:b:"" | a <- chrFileList, b <- chrRankList]
squares = [Square f r | f <- files, r <- ranks]

instance Arbitrary Square where
  arbitrary = elements squares             

prop_success_read_file :: Char -> Bool
prop_success_read_file c = if elem (toLower c) chrFileList
  then showFile (fromJust $ readCFile c) == (toLower c)
  else isNothing (readCFile c)

prop_success_read_rank :: Char -> Bool
prop_success_read_rank c = if elem c chrRankList
  then showRank (fromJust $ readRank c) == c
  else isNothing (readRank c)

prop_show_square :: Square -> Bool
prop_show_square sq = elem (show sq) strSquareList

prop_read_square :: String -> Bool
prop_read_square str = if  isNothing (readSquare str)  
  then not (is_str str)  else is_sq str
  where is_sq s = elem (fromJust (readSquare (map toUpper s)))
          squares
        is_str s = elem (take 2 (map toUpper s)) strSquareList

prop_int_to_square :: Int -> Bool
prop_int_to_square n | or [n < 0, n > 63] = chkNo
                     | otherwise = chkSq 
  where chkNo = isNothing $ intToSquare n
        chkSq = elem (fromJust $ intToSquare n) squares

prop_int100_to_square :: Int -> Bool
prop_int100_to_square n | or [n < 11 , n > 89] = chkNo
                        | or [n `mod` 10 == 0, n `mod` 10 == 9] = chkNo 
                        | otherwise = chkSq 
  where chkNo = isNothing $ int100ToSquare n
        chkSq = elem (fromJust $ int100ToSquare n) squares

prop_square_to_int64 :: Square -> Bool
prop_square_to_int64 sq | or [chkInt >= 0, chkInt < 64] = True
                        | otherwise = False
  where chkInt = squareToInt64 sq

prop_square_to_int100 :: Square -> Bool
prop_square_to_int100 sq | and [or [chkInt >= 11, chkInt < 89],
  or [chkInt `mod` 10 /= 0, chkInt `mod` 10 /= 9]] = True
                         | otherwise = False
  where chkInt = squareToInt100 sq

return []
runTests :: IO Bool
runTests = $forAllProperties
  (quickCheckWithResult stdArgs {maxSuccess = 500})

main :: IO ()
main = do
         x <- runTests
         if x then
           do
             return ()
         else
           do
             Exit.exitFailure
