{-# LANGUAGE TemplateHaskell #-}
import Squares
import Test.QuickCheck.All
import Test.QuickCheck 
import Data.Char
import Data.Maybe (isNothing)
import qualified System.Exit as Exit
import Control.Monad(unless)


prop_failureReadRank :: Char -> Property
prop_failureReadRank c = isNothing (readRank c)
  ==> notElem (toLower c) rank_chrs

prop_failureReadFile :: Char -> Property
prop_failureReadFile c = isNothing (readCFile c)
  ==> notElem (toLower c) file_chrs


return []
runTests :: IO Bool
runTests = $forAllProperties
  (quickCheckWithResult stdArgs {maxSuccess = 200})

main :: IO ()
main = do
         x <- runTests
         unless x Exit.exitFailure

