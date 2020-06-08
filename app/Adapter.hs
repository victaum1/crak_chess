module Adapter where

import System.Console.Readline
import Engine

ui_loop :: Play_Args -> IO()
ui_loop args = do
            line <- readline ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              otherwise -> do
                putStrLn "Error: Command not known!"
                ui_loop args

uci_loop :: IO ()
uci_loop = do
             putStrLn "uciok"
             ui_loop init_args

xb_loop :: Play_Args -> IO ()
xb_loop args = do
            line <- readline ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              otherwise -> do 
                   putStrLn "Error: Command not known!"
                   xb_loop init_args

xboard_loop :: IO ()
xboard_loop = do
  putStrLn ""
  xb_loop init_args
