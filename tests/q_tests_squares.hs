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
prop_success_read_file c = if elem (toUpper c) chrFileList
  then show (fromJust $ readCFile c) == toUpper c : "_F"
  else isNothing (readCFile c)

prop_success_read_rank :: Char -> Bool
prop_success_read_rank c = if elem c chrRankList
                           then show (fromJust $ readRank c) ==
                             "R"++[c]
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

prop_int120_to_square :: Int -> Bool
prop_int120_to_square n | or [n < 21 , n > 98] = chkNo
                        | or [n `mod` 10 == 0, n `mod` 10 == 9] =
                          chkNo 
                        | otherwise = chkSq 
  where chkNo = isNothing $ int120ToSquare n
        chkSq = elem (fromJust $ int120ToSquare n) squares

prop_square_to_int64 :: Square -> Bool
prop_square_to_int64 sq | or [chkInt >= 0, chkInt < 64] = True
                        | otherwise = False
  where chkInt = squareToInt64 sq

prop_square_to_int120 :: Square -> Bool
prop_square_to_int120 sq | and [or [chkInt >= 21, chkInt < 99],
  or [chkInt `mod` 10 /= 0, chkInt `mod` 10 /= 9]] = True
                         | otherwise = False
  where chkInt = squareToInt120 sq

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
