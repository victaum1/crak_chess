module Uci where

import Data.Either
import Control.Monad.Trans.State
import System.Random
import Data.Maybe
import Defs
import Parsing
import Moves
import Game
import SubEngine
import Engine
import Utils

-- var
uci_info = unlines ["id name " ++ name ++ " " ++ version,"id author " ++ author, "uciok"]

ui_map :: [(String, [String] -> StateT PlayArgs IO())]
ui_map = [
   ("isready", const $ mio (putStrLn "readyok"))
  ,("ucinewgame", const uNew)
  ,("stop", const stop)
  ,("position", setUpos)
  ,("dump", const mDump)
  ,("dumpfen", const mDumpFEN)
  ,("dumpplay", const mDumpPlay)
  ,("go", const uGo)
  ,("uci", const uciOk)
         ]

stop = do
  args <- get
  let arg_ = args{getCpFlag=Nothing}
  put arg_

uNew = do
       args <- get
       let arg_ = init_args{getCpFlag=Nothing ,getProtocol=False}
       put arg_


uciOk :: StateT PlayArgs IO ()
uciOk = do
        mio (putStrLn "uciok")

comUci :: String -> StateT PlayArgs IO ()
comUci line = do
--            mio $ putStrLn "In comUci..."
            if null line then endOfLine
              else do
                     let input = words line
                     let cmd = head input
                     let args = tail input
                     let res = lookup cmd ui_map
                     maybe (mio (errorCmd ["unknown command"
                                          , unwords input]))
                           (\a -> a args) res


uGo :: StateT PlayArgs IO ()
uGo = do
  args <- get
  let g = getGame args
  let s = turn g
  let arg_ = args{getCpFlag=Just s}
  put arg_


pIpos = do
  symbol "startpos"
  m <- symbol "moves"
  s <- getInput
  return (m,s)

pFg = do
  symbol "fen"
  g <- pGame
  s <- getInput
  return (g,s)


setUpos :: [String] -> StateT PlayArgs IO ()
setUpos [] = mio $ errorCmd ["incomplete", []]
setUpos [s] = do
  let p = parse (string "startpos") "" s
  if isLeft p then mio $ errorCmd ["unknown command", s]
  else mSetPosition init_fen
setUpos (s:ss) = do
  let input = unwords (s:ss)
  let pinp0 = parse pIpos "" input
  let pinp1 = parse pFg "" input
  if isLeft pinp0 && isLeft pinp1 then mio $ errorCmd ["unknown command",
    input]
  else if isLeft pinp0 then do
         let g = myRight (fst <$> pinp1)
         let ns = snd <$> pinp1
         if (null <$> ns) == Right True then do
           mSetPosition $ game2FEN g
         else do
           let pms = myRight $ words <$> ns
           let hmoves = head pms
           let tmoves = tail pms
           if hmoves == "moves" then do
             let movs = mapM readMove tmoves
             either (const $ mio $ errorCmd ["not moves", input])
                   (\m -> do
                     setPosWithMoves g m) movs
           else mio $ errorCmd ["unknown command", input]
       else do
         let ns = snd <$> pinp0
         if isLeft ns then mio $ errorCmd ["incomplete", input]
         else do
            let mvs = words $ myRight ns
            let movs = mapM readMove mvs
            either (const $ mio $ errorCmd ["not moves", input])
                  (\m -> do
                      setPosWithMoves init_game m) movs


setPosWithMoves :: Game -> [Move] -> StateT PlayArgs IO ()
setPosWithMoves g ms = do
  iargs <- get
  put $ setGame g iargs
  setListMoves ms


setListMoves :: [Move] -> StateT PlayArgs IO ()
setListMoves []     = return ()
setListMoves [m]    = mMakeMove m
setListMoves (m:ms) = do
  mMakeMove m
  setListMoves ms
