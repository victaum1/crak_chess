{-# LANGUAGE TemplateHaskell #-}
import Squares
import Test.QuickCheck.All
import Test.QuickCheck 
import Data.Char
import Data.Maybe (fromJust, isNothing)
import qualified System.Exit as Exit

strSquareList = [a:b:"" | a <- chrFileList, b <- chrRankList]
squareList = [Square (fromJust $ readCFile f) (fromJust $ readRank r)
  | f <- chrFileList, r <- chrRankList]

instance Arbitrary Square where
  arbitrary = elements squareList             

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
          squareList
        is_str s = elem (take 2 (map toUpper s)) strSquareList

return []
runTests :: IO Bool
runTests = $forAllProperties
  (quickCheckWithResult stdArgs {maxSuccess = 500})

main :: IO ()
main = do
         x <- runTests
         if x then
           do
             putStrLn "All tests passed! :)"
             return ()
         else
           do
             putStrLn "Some tests failed! :|"
             Exit.exitFailure
