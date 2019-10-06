module Main where

import Data.Maybe
import System.IO
import System.Console.Readline
import Adapter

main_loop = do
              maybeline <- readline ""
              case maybeline of
                Nothing       -> return () -- EOF / ctrl-d
                Just "quit"   -> return ()
                Just "xboard" -> xboard_loop
                Just "uci"    -> uci_loop
                Just line     -> do
                   putStrLn $ "Error (unknown command) : " ++ line
                   main_loop
              


main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         putStrLn "Craken 0.9.x by V. Manotas"
         main_loop
