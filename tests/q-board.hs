{-# LANGUAGE TemplateHaskell #-}
import Board
import Test.QuickCheck
import qualified System.Exit as Exit
import Control.Monad(unless)
import Parsing
import Data.Map.Strict as Map hiding (null)


-- failing on readBoard
prop_read_board_fail str = readBoard str == empty_board ==> (null . parse pBoard) str

-- readBoard and showBoard are reverse
genSafeCChar :: Gen Char
genSafeCChar = elements ".pnbrqkPNBRQK"

genSafeCString = fmap unlines (mapM sequence [ [genSafeCChar | _ <-[1..8]] | _ <-[1..8]])

newtype SafeCString = SafeCString {getCString::String}
  deriving Show

instance Arbitrary SafeCString where
  arbitrary = SafeCString <$> genSafeCString

prop_read_board_8by8 :: SafeCString -> Bool
prop_read_board_8by8 vstr = showBoard (readBoard a_vstr) ==
  a_vstr
  where a_vstr = getCString vstr

-- running tests
return []
runTests :: IO Bool
runTests = $forAllProperties
  (quickCheckWithResult stdArgs {maxSuccess = 500})

main :: IO ()
main = do
         x <- runTests
         unless x Exit.exitFailure
