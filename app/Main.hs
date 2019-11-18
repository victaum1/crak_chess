module Main where

import System.IO
import System.Console.Haskeline
import Adapter

version = "0.0.10.6"
help_str = unlines [
  "play  - Engine thinks and plays for the current turn."
  ,"stop - Engine Stops."
  ,"st n - Sets the time per move in seconds."
  ,"sd n - Sets the deph of searching."
  ,"undo - Undo a move."
  ,"new - New game."
  ,"dump - Dumps the board."
  ,"quit - Exits the engine."
  ,"xboard - Switch to xboard protocol."
  ,"uci - Switch to uci protocol."
  ]

main_loop = do
              maybeline <- getInputLine "crak> "
              case maybeline of
                Nothing       -> return () -- EOF / ctrl-d
                Just "quit"   -> return ()
                Just "xboard" -> xboard_loop
                Just "uci"    -> uci_loop
                Just "help" -> do
                  outputStrLn help_str
                  main_loop 
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
           outputStrLn $ "Craken " ++ version ++ " by V. Manotas."
           outputStrLn "x/x/2019."
           outputStrLn ""
           outputStrLn "'help' show usage."
           main_loop
