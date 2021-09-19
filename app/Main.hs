module Main where

import Control.Monad.Trans.State ( StateT, evalStateT, get, put )
import Defs ( author, date, errorCmd, mio, quit, version )
import Engine ( init_args, PlayArgs(getHist, getGame, getCpFlag, getSeed, getProt) )
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
import Data.Maybe ( fromMaybe, isNothing, fromJust )
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
  ,("new", const $ mNew)
  ,("undo", const playUndo)
  ,("dump", const mainDump)
  ,("help", const helpPlay)
  ,("quit", const quitPlay)
  ,("xboard", const $ mio $ xboardLoop init_args)
  ,("uci", const $ mio mUci)
  ,("setposition", setPos)
  ,("sp", setPos)
  ,("dumpfen", const (mDumpFEN >> playLoop))
  ,("dumpplay", const (mDumpPlay >> playLoop))
  ]


mUci = do
  let args = init_args{getProt=False}
  uciLoop args


mNew = do
       args <- get
       let arg_ = init_args{getSeed=getSeed args}
       put arg_
       playLoop


playLoop = do
  line <- mio $ getPlay "play> "
  if null line then playLoop
  else do
    let input = words line
    let cmd = head input
    let args = unwords $ tail input
    let a_move = parse pMoveCoord cmd
    if isNothing a_move then do
        let res = lookup cmd play_map
        maybe (mio (errorCmd ["unknown command", unwords input]) >>
          playLoop)
          (\a -> a args) res
    else do
      let m = fromJust $ fst <$> a_move
      mMakeMove m
      pargs <-get
      let cpf = getCpFlag pargs
      let s = turn $ getGame pargs
      if Just s == cpf then do
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
  let arg_ = args{getCpFlag=Just s}
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


main_map :: [(String, PlayArgs -> IO ())]
main_map = [
  ("quit", const quit)
  ,("help", mainHelp)
  ,("xboard", xboardLoop)
  ,("uci", const mUci)
  ,("play", mainPlay)
  ]


mainHelp pa = do
  putStr help_str
  mainLoop pa


mainPlay = evalStateT playLoop


getPlay caller = do
  putStr caller
  getLine


mainLoop :: PlayArgs -> IO ()
mainLoop pa = do
  res <- getPlay ""
  if null res then mainLoop pa
  else do
    let mbAction = lookup res main_map
    fromMaybe (putStrLn "" >> mainLoop pa)
          (mbAction <*> Just pa)

help_cmd_args = unlines [
    "-s <n> [<playFlag>]"
  , "Sets Radom Number Seed."
  , "-x"
  , "playFlag for Xboard."
  , "-u"
  , "playFlag for UCI."
                        ]

setSeed :: Int -> PlayArgs
setSeed n = init_args{getSeed=Just n}

parseArgs :: [String] -> IO()
parseArgs ["-s", n, "-x"] = xboardLoop (setSeed (read n))
parseArgs ["-s", n, "-u"] = uciLoop (setSeed (read n))
parseArgs ["-s", n] = mainLoop (setSeed (read n))
parseArgs ["-x"] = xboardLoop init_args
parseArgs ["-u"] = uciLoop init_args
parseArgs ["-h"] = putStr help_cmd_args
parseArgs [] = mainLoop init_args
parseArgs ss = putStr "Invalid args: see '-h'."


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Craken " ++ version ++ " " ++ author
  putStrLn $ date ++ "."
  putStrLn "'help' show usage."
  args <- getArgs
  parseArgs args
