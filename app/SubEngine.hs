module SubEngine where

import Control.Monad.Trans.State
import System.Random
import Engine
import Moves
import Defs
import Game
import Pieces
import Valid
import Play
import Generator


mTakeBack :: StateT PlayArgs IO ()
mTakeBack = do
  args <- get
  let args_ = takeBack args
  put args_


mAdjudicate :: StateT PlayArgs IO ()
mAdjudicate = do
  args <- get
  let a_game = getGame args
  let a_side = turn a_game
  if isInCheck a_game then do
    if a_side then do
      mio $ putStrLn "result 1-0 {White mates}"
    else do
      mio $ putStrLn "result 0-1 {Black mates}"
  else do
    mio $ putStrLn "result 1/2-1/2 {Stalemate}"


mThinkMove :: StateT PlayArgs IO ()
mThinkMove = do
  gen  <- newStdGen 
  args <- get
  let a_game = getGame args
  let a_move = think a_game gen
  maybe mAdjudicate (
    \m -> do
      mMakeMove m
      mio $ putStrLn $ "move " ++ show m
    ) a_move


mMakeMove :: Move -> StateT PlayArgs IO ()
mMakeMove m = do
  args <- get
  let a_hist = getHist args
  let a_game = getGame args
  let n_game = makeMove m a_game
  maybe (mio $ putStrLn $ "ILLegal move: " ++ show m) (
    \g -> do
      let o_hist = a_game:a_hist
      let args_ = args{ getGame = g, getHist = o_hist}
      put args_
           ) n_game


mSetPosition :: String -> StateT PlayArgs IO ()
mSetPosition strs = do
  pargs <- get
  let a_pos = fen2Game strs
  maybe (mio $ errorCmd ["not a FEN position!", strs])
    (\res -> do
        let o_args = setGame res pargs
        put o_args
    ) a_pos


mDump :: StateT PlayArgs IO ()
mDump = do
  args <- get
  mio $ putStrLn $ dump args

-- for debugging
mDumpFEN :: StateT PlayArgs IO ()
mDumpFEN = do
  args <- get
  mio $ putStrLn $ dumpFEN args

mDumpPlay :: StateT PlayArgs IO ()
mDumpPlay = do
  args <- get
  mio $ putStrLn $ dumpPlay args
