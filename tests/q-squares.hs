{-# LANGUAGE TemplateHaskell #-}
import Squares
import Test.QuickCheck.All
import Test.QuickCheck 
import Data.Char
import Data.Maybe (fromJust, isNothing, isJust)
import qualified System.Exit as Exit


prop_failureReadRank :: Char -> Property
prop_failureReadRank c = isNothing (readRank c)
  ==> not (elem (toLower c) chr_rank_ls)

prop_failureReadFile :: Char -> Property
prop_failureReadFile c = isNothing (readCFile c)
  ==> not (elem (toLower c) chr_file_ls)


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

