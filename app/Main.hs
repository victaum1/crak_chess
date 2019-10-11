module Main where

-- import Data.Maybe
import System.IO
import System.Console.Haskeline
import Adapter

main_loop = do
              maybeline <- getInputLine "> "
              case maybeline of
                Nothing       -> return () -- EOF / ctrl-d
                Just "quit"   -> return ()
                Just "xboard" -> xboard_loop
                Just "uci"    -> uci_loop
                Just line     -> do
                   outputStrLn $ "Error (unknown command) : " ++ line
                   main_loop

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         runInputT defaultSettings entry
       where
         entry :: InputT IO ()
         entry = do
           outputStrLn "Craken 0.9.x by V. Manotas"
           main_loop
