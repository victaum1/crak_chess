{-# LANGUAGE TemplateHaskell #-}
import Board 
import Test.QuickCheck 
-- import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing)
import qualified System.Exit as Exit

genSafeCChar :: Gen Char
genSafeCChar = elements ".pnbrqkPNBRQK"

genSafeCString = fmap unlines (mapM sequence [ [genSafeCChar|_<-[1..8]] | _ 
  <-[1..8]])

newtype SafeCString = SafeCString {getCString::String} 
  deriving Show

instance Arbitrary SafeCString where
  arbitrary = SafeCString <$> genSafeCString 

prop_read_board_8by8 :: SafeCString -> Bool
prop_read_board_8by8 vstr = showBoard (fromJust $ readBoard a_vstr) ==
  a_vstr
  where a_vstr = getCString vstr

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
