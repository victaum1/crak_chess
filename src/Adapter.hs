module Adapter where

ui_loop = do
            line <- getLine
            case line of
              "quit" -> return ()
              otherwise -> do 
                   putStrLn "Error: Command not known!"
                   ui_loop

uci_loop = do
             putStrLn "uciok"
             ui_loop

xb_loop = do
            line <- getLine
            case line of
              "quit" -> return ()
              otherwise -> do 
                   putStrLn "Error: Command not known!"
                   xb_loop

xboard_loop = do
                putStr "\n"
                xb_loop
