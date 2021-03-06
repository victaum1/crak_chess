module Main where

import Control.Monad.Trans.State ( evalStateT, get, StateT )
import Defs ( version, quit, errorCmd, mio )
import Engine ( init_args, PlayArgs(getHist) )
import Moves ( pMoveCoord )
import Parsing ( parse )
import SubEngine
    ( mDump,
      mTakeBack,
      mThinkMove,
      mSetPosition,
      mMakeMove,
      mDumpPlay,
      mDumpFEN )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import           Uci                       (uciLoop)
import           Xboard                    (xboardLoop)


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
  line <- mio $ getPlay "play"
  if null line then playLoop
  else do
    let input = words line
    let cmd = head input
    let args = unwords $ tail input
    let a_move = parse pMoveCoord cmd
    if null a_move then do
        let res = lookup cmd play_map
        maybe (mio (errorCmd ["unknown command", unwords input]) >> playLoop)
          (\a -> a args) res
    else do
      mMakeMove $ fst $ head a_move
      playLoop

setPos :: String -> StateT PlayArgs IO ()
setPos strs = mSetPosition strs >> playLoop


playGo = mThinkMove >> playLoop
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
  putStr $ caller ++ "> "
  getLine



mainLoop :: IO ()
mainLoop = do
  res <- getPlay "Craken"
  if null res then mainLoop
  else do
    let mbAction = lookup res main_map
    maybe (putStrLn "" >> mainLoop)
          id mbAction

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Craken " ++ version ++ " by V. Manotas."
  putStrLn "x/x/2020."
  putStrLn "'help' show usage."
  putStrLn ""
  mainLoop

