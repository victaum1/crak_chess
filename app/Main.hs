module Main where

import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import System.Environment (getArgs)
import Control.Monad.Trans.State ( StateT (runStateT), evalStateT, evalState, get, put )
import Defs (author, version, name, date, mio, quit, errorCmd, endOfLine)
import Engine (PlayArgs, getSeed, init_args, getProtocol, getCpFlag, getGame, getHist)
import Data.Maybe (fromMaybe, isNothing)
import Moves (pMoveCoord)
import Parsing (parse)
import Data.Either (isLeft)
import Utils (myRight)
import SubEngine (mMakeMove, mTakeBack, mDump, mSetPosition, mDumpFEN, mDumpPlay)
import Game (turn)
import IoSearch (ioSearch)
import Xboard (comXboard)
import Uci (comUci, uci_info)

-- vars

play_map :: [(String, String -> StateT PlayArgs IO ())]
play_map = [
  ("play", const playGo)
  ,("stop", const stop)
  ,("new", const mNew)
  ,("undo", const playUndo)
  ,("dump", const mainDump)
  ,("help", const helpPlay)
  ,("setposition", setPos)
  ,("sp", setPos)
  ,("dumpfen", const mDumpFEN)
  ,("dumpplay", const mDumpPlay)
  ]

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


main_map :: [(String, PlayArgs -> IO ())]
main_map = [
  ("quit", const quit)
  ,("help", const mainHelp)
  ,("xboard", \pa -> evalStateT eventLoop (Just True,pa))
  ,("uci"   , \pa -> do
       putStr uci_info
       evalStateT eventLoop (Just False,pa{getCpFlag=Nothing}))
  ,("play"  , \pa -> evalStateT eventLoop (Nothing,pa)) -- comPlay
  ]

-- funcs

comPlay :: String -> StateT PlayArgs IO ()
comPlay line = do
  mio $ putStrLn "play>"
  if null line then endOfLine -- keep going
  else do
    let input = words line
    let cmd = head input
    let args = unwords $ tail input
    let a_move = parse pMoveCoord "" cmd
    if isLeft a_move then do
        let res = lookup cmd play_map
        maybe (mio (errorCmd ["unknown command", unwords input]))
          (\a -> a args) res
    else do
      let m = myRight a_move
      mMakeMove m


playGo = do
  args <- get
  let g = getGame args
  let s = turn g
  let arg_ = args{getCpFlag=Just s}
  put arg_


stop = do
  args <- get
  put args{getCpFlag=Nothing}


mNew = do
       args <- get
       let arg_ = init_args{getSeed=getSeed args}
       put arg_


playUndo = do
  args <- get
  let a_hist = getHist args
  if length a_hist < 2 then mio $ print ""
    else do
           mTakeBack
           mTakeBack


mainDump :: StateT PlayArgs IO ()
mainDump = do
  mDump


helpPlay = do
  mio $ putStr help_str


setPos :: String -> StateT PlayArgs IO ()
setPos = mSetPosition


eventLoop :: StateT (Maybe Bool,PlayArgs) IO ()
eventLoop = do
  mio $ print "In the event loop..."
  (mb,pa) <- get
  if isNothing mb then do
    mio $ putStr "play> "
    input <- mio getLine
    if input == "quit" then mio quit else do
      (_,opa) <- mio $ runStateT (comPlay input >> ioSearch) pa
      put (mb,opa)
      eventLoop
  else do
    if mb == Just True then do
      input <- mio getLine
      if input == "quit" then mio quit else do
        (_,opa) <- mio $ runStateT (comXboard input >> ioSearch) pa
        put (mb,opa)
        eventLoop
    else do
      input <- mio getLine
      if input == "quit" then mio quit else do
        (_,opa) <- mio $ runStateT (comUci input >> ioSearch) pa
        put (mb,opa)
        eventLoop


mainHelp :: IO ()
mainHelp = do
  putStr help_str
  mainLoop init_args


mainLoop :: PlayArgs -> IO ()
mainLoop pa = do
  -- print "In the main loop..."
  res <- getLine
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

setProt :: Bool -> PlayArgs
setProt b | b = init_args{getProtocol=True}
          | otherwise = init_args{getProtocol=False}

parseArgs :: [String] -> IO ()
parseArgs ["-s", n, "-x"] = mainLoop (setSeed (read n))
parseArgs ["-s", n, "-u"] = mainLoop (setSeed (read n))
parseArgs ["-s", n] =  mainLoop (setSeed (read n))
parseArgs ["-x"] =  mainLoop (setProt True)
parseArgs ["-u"] = do
  putStrLn uci_info
  putStrLn "uciok"
  mainLoop (init_args{getProtocol = False, getCpFlag = Nothing})
parseArgs ["-h"] = putStr help_cmd_args
parseArgs [] = mainLoop init_args
parseArgs ss = putStr "Invalid args: see '-h'."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ name ++ " " ++ version ++ " by " ++ author ++ "."
  putStrLn $ date ++ "."
  putStrLn "'help' show usage."
  args <- getArgs
  parseArgs args
