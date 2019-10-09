{-# LANGUAGE TemplateHaskell #-}
import Pieces 
import Test.QuickCheck 
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing)
import qualified System.Exit as Exit

pieceList = [Piece s p | s <- [White, Black], p <- pieceTypeList]
validChar = "." ++ pieceCharList

instance Arbitrary Piece where
  arbitrary = elements pieceList

prop_read_piece :: Char -> Bool
prop_read_piece c | readCPiece c == Nothing = True
                  | readCPiece c == Just Nothing = True 
                  | elem (fromJust(fromJust(readCPiece c))) pieceList
                    = True
                  | otherwise = False

prop_show_piece :: Piece -> Bool
prop_show_piece p = elem (showPiece p) $ (map toLower pieceCharList) ++
  pieceCharList

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
