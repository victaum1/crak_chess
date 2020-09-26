module Adapter where

import           Engine
import           System.Console.Readline

uiLoop :: PlayArgs -> IO()
uiLoop args = do
            line <- readline ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              _ -> do
                putStrLn "Error: Command not known!"
                uiLoop args

uciLoop :: IO ()
uciLoop = do
             putStrLn "uciok"
             uiLoop init_args

xbLoop :: PlayArgs -> IO ()
xbLoop args = do
            line <- readline ""
            case line of
              Nothing -> return ()
              Just "quit" -> return ()
              _ -> do
                   putStrLn "Error: Command not known!"
                   xbLoop init_args

xboardLoop :: IO ()
xboardLoop = do
  putStrLn ""
  xbLoop init_args
