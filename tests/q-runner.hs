module Main (main) where
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import qualified System.Exit as Exit

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse xs == xs

runTests :: IO Bool
runTests = do
             result <- quickCheckWithResult stdArgs {maxSuccess = 500}
               prop_reverse
             return (isSuccess result)

main :: IO ()
main = do
         result <- runTests
         if result then
           do
             putStrLn "All tests passed! :)"
             return ()
         else
           do
             putStrLn "Some test failed!: :|"
             Exit.exitFailure
