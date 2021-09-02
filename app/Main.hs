module Main where

-- import Control.Monad.Trans.State
-- import Defs
-- import Engine
-- import Moves
-- import Parsing
-- import SubEngine
-- import System.IO
-- import System.Enviroment
-- import Uci
-- import Xboard
-- import Data.Maybe
-- import Game

import Control.Monad.Trans.State ( StateT, evalStateT, get )
import Defs ( errorCmd, mio, quit, version )
import Engine ( init_args, PlayArgs(getHist, getGame, getCpFlag) )
import Moves ( pMoveCoord )
import Parsing ( parse )
import SubEngine
    ( mDump,
      mDumpFEN,
      mDumpPlay,
      mMakeMove,
      mSetPosition,
      mTakeBack,
      mThinkMove )
import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout )
import System.Environment ( getArgs )
import Uci ( uciLoop )
import Xboard ( xboardLoop )
import Data.Maybe ( fromMaybe )
import Game ( GameState(turn) )

help_str = unlines [
  "quit - Exits the engine."
  ,"uci - Switch to uci protocol."
  ,"xboard - Switch to xboard protocol."
  ,"play - play Mode"
  ,"In play Mode: "
  ,"play  - Engine thinks and plays for the current turn."
  ,"stop - Engine Stops (Human vs Human)."
  ,"st n - Sets the time per move in seconds."
  ,"sd n - Sets the deph of searching."
  ,"undo - Undo a move."
  ,"new - New game."
  ,"dump - Dumps the board."
  ,"Enter moves in coordinate notation. Eg: 'e2e4', 'a7a8Q'"
  ]


play_map :: [(String, String -> StateT PlayArgs IO ())]
play_map = [
  ("play", const playGo)
  ,("stop", const stop)
  ,("new", const $ mio mainPlay)
  ,("undo", const playUndo)
  ,("dump", const mainDump)
  ,("help", const helpPlay)
  ,("quit", const quitPlay)
  ,("xboard", const $ mio xboardLoop)
  ,("uci", const $ mio uciLoop)
  ,("setposition", setPos)
  ,("sp", setPos)
  ,("dumpfen", const (mDumpFEN >> playLoop))
  ,("dumpplay", const (mDumpPlay >> playLoop))
  ]


playLoop = do
  line <- mio $ getPlay "play> "
  if null line then playLoop
  else do
    let input = words line
    let cmd = head input
    let args = unwords $ tail input
    let a_move = parse pMoveCoord cmd
    if null a_move then do
        let res = lookup cmd play_map
        maybe (mio (errorCmd ["unknown command", unwords input]) >>
          playLoop)
          (\a -> a args) res
    else do
      let m = fst $ head a_move
      mMakeMove m
      pargs <-get
      let cpf = getCpFlag pargs
      let s = turn $ getGame pargs
      if s == cpf then do
         mThinkMove
         playLoop
      else
        playLoop


setPos :: String -> StateT PlayArgs IO ()
setPos strs = mSetPosition strs >> playLoop


playGo = do
  args <- get
  let g = getGame args
  let s = turn g
  let arg_ = args{getCpFlag=s}
  mThinkMove
  playLoop


stop = playLoop


playUndo = do
  args <- get
  let a_hist = getHist args
  if length a_hist < 2 then playLoop
    else do
           mTakeBack
           mTakeBack
           playLoop


mainDump :: StateT PlayArgs IO ()
mainDump = do
  mDump
  playLoop


helpPlay = do
  mio $ putStr help_str
  playLoop

quitPlay = mio quit


main_map :: [(String, IO ())]
main_map = [
  ("quit", quit)
  ,("help", mainHelp)
  ,("xboard", xboardLoop)
  ,("uci", uciLoop)
  ,("play", mainPlay)
  ]


mainHelp = do
  putStr help_str
  mainLoop


mainPlay = evalStateT playLoop init_args


getPlay caller = do
  putStr $ caller
  getLine



mainLoop :: IO ()
mainLoop = do
  res <- getPlay ""
  if null res then mainLoop
  else do
    let mbAction = lookup res main_map
    fromMaybe (putStrLn "" >> mainLoop)
          mbAction

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Craken " ++ version ++ " by V. Manotas."
  putStrLn "x/x/2020."
  putStrLn "'help' show usage."
  args <- getArgs
  if null args then mainLoop else
    if head args == "-x" then xboardLoop
    else if head args == "-u" then uciLoop
    else mainLoop
