module Adapter where

ui_loop = do
            line <- getLine
            case line of
              "quit" -> return ()
              otherwise -> do 
                   putStrLn "Error: Command not known!"
                   do ui_loop

uci_loop = do
             putStrLn "uciok"
             do ui_loop

xb_loop = do
            line <- getLine
            case line of
              "quit" -> return ()
              otherwise -> do 
                   putStrLn "Error: Command not known!"
                   do xb_loop

xboard_loop = do
                putStr "\n"
                do xb_loop
