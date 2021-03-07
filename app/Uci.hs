module Uci where

import Defs(mio, quit, version)
import Engine
-- import System.Console.Readline
import Control.Monad.Trans.State

-- var
ui_map :: [(String, [String] -> StateT PlayArgs IO())]
ui_map = [
   ("quit", const $ mio quit)
  ,("isready", \_ -> do
       mio $ putStrLn "readyok"
       uiLoop)
  ,("ucinewgame", const $ mio $ evalStateT uiLoop init_args)
  ,("stop", \_ -> do
       mio $ putStrLn "bestmove 0000"
       uiLoop)
         ]


uiLoop :: StateT PlayArgs IO ()
uiLoop = do
            line <- mio getLine
            if null line then uiLoop
              else do
                     let input = words line
                     let cmd = head input
                     let args = tail input
                     let res = lookup cmd ui_map
                     maybe (mio (errorCmd ["unknown command", unwords input]) >> uiLoop)
                       (\a -> a args) res


uciLoop :: IO ()
uciLoop = do
             putStrLn $ "id name Craken " ++ version
             putStrLn "id author V. Manotas"
             putStrLn "uciok"
             evalStateT uiLoop init_args

