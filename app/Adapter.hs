module Adapter where

import System.Console.Haskeline

ui_loop = do
            line <- getInputLine ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              otherwise -> do
                outputStrLn "Error: Command not known!"
                ui_loop

uci_loop :: InputT IO ()
uci_loop = do
             outputStrLn "uciok"
             ui_loop

xb_loop = do
            line <- getInputLine ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              otherwise -> do 
                   outputStrLn "Error: Command not known!"
                   xb_loop

xboard_loop :: InputT IO ()
xboard_loop = do
                outputStrLn ""
                xb_loop
