module Uci where

import Parsing
import Board
import Moves
import Game
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
  ,("position", \ss -> setUpos ss >> uiLoop)
  ,("dump", const uDump)
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
                     maybe (mio (errorCmd ["unknown command", unwords input]) >>
                            uiLoop)
                       (\a -> a args) res

-- debugging
uDump = do
  dump
  uiLoop


setUpos :: [String] -> StateT PlayArgs IO ()
setUpos [] = mio $ errorCmd ["incomplete", []]
setUpos [s] = do
  let p = parse (symbol "startpos") s
  if null p then mio $ errorCmd ["unknown command", s]
  else setPosition init_fen
setUpos (s:ss) = do
  let input = unwords (s:ss)
  let pinp0 = parse (symbol "startpos" >> symbol "moves" ) input
  let pinp1 = parse pGame input
  if null pinp0 && null pinp1 then mio $ errorCmd ["unknown command", input]
  else if null pinp0 then do
         let g = fst $ head pinp1
         let ns = snd $ head pinp1
         if null ns then do
           setPosition $ game2FEN g
         else do
           let pms = words ns
           let hmoves = head pms
           let tmoves = tail pms
           if hmoves == "moves" then do
             let movs = mapM readMove tmoves
             maybe (mio $ errorCmd ["not moves", input])
                   (\m -> do
                     setPosWithMoves g m) movs
           else mio $ errorCmd ["unknown command", input]
       else do
         let ns = snd $ head pinp0
         if null ns then mio $ errorCmd ["incomplete", input]
         else do
            let mvs = words ns
            let movs = mapM readMove mvs
            maybe (mio $ errorCmd ["not moves", input])
                  (\m -> do
                      setPosWithMoves init_game m) movs


setPosWithMoves :: Game -> [Move] -> StateT PlayArgs IO ()
setPosWithMoves g ms = do
  iargs <- get
  put $ setGame g iargs
  setListMoves ms


setListMoves :: [Move] -> StateT PlayArgs IO ()
setListMoves []     = return ()
setListMoves [m]    = xMakeMove m
setListMoves (m:ms) = do
  xMakeMove m
  setListMoves ms


uciLoop :: IO ()
uciLoop = do
             putStrLn $ "id name Craken " ++ version
             putStrLn "id author V. Manotas"
             putStrLn "uciok"
             evalStateT uiLoop init_args

