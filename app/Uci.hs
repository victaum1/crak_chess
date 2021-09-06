module Uci where
import Control.Monad.Trans.State ( evalStateT, get, put, StateT )
import System.Random (StdGen, newStdGen, mkStdGen )
import Data.Maybe (isNothing,fromJust)
import Defs ( version, author, name, quit, mio, errorCmd )
import Parsing ( parse, symbol )
import Moves ( readMove, Move )
import Game ( game2FEN, init_fen, init_game, pGame, Game )
import SubEngine
    ( mMakeMove, mSetPosition, mDump, mDumpFEN, mDumpPlay )
import Engine ( think, setGame, init_args, PlayArgs(getGame,getSeed) )

-- var
uci_info = unlines ["id name " ++ name ++ " " ++ version,"id author " ++ author, "uciok"]

ui_map :: [(String, [String] -> StateT PlayArgs IO())]
ui_map = [
   ("quit", const $ mio quit)
  ,("isready", const $ mio (putStrLn "readyok")>>uiLoop)
  ,("ucinewgame", const $ uNew)
  ,("stop", const $ mio (putStrLn "bestmove 0000")>>uiLoop)
  ,("position", \ss -> setUpos ss >> uiLoop)
  ,("dump", const (mDump >> uiLoop))
  ,("dumpfen", const (mDumpFEN >> uiLoop))
  ,("dumpplay", const (mDumpPlay >> uiLoop))
  ,("go", const uGo)
  ,("uci", const uciInfo)
         ]
         
uNew = do
       args <- get
       let arg_ = init_args{getSeed=getSeed args}
       put arg_
       uiLoop

uciInfo :: StateT PlayArgs IO ()
uciInfo = do
        mio (putStrLn uci_info)
        uiLoop

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


uGo :: StateT PlayArgs IO ()
uGo = do
  infoPost
  uThink
  uiLoop

infoPost = do
  mio $ putStrLn "info depth 1 score cp 0 time 1 pv 0000"

uThink = do
  args <- get
  g <- newStdGen
  let g_ = maybe g mkStdGen (getSeed args)
  let a_game = getGame args
  let a_move = think a_game g_
  maybe (mio $ putStrLn "bestmove 0000") (
    \m -> do
      mio $ putStrLn $ "bestmove " ++ show m
    ) a_move

setUpos :: [String] -> StateT PlayArgs IO ()
setUpos [] = mio $ errorCmd ["incomplete", []]
setUpos [s] = do
  let p = parse (symbol "startpos") s
  if null p then mio $ errorCmd ["unknown command", s]
  else mSetPosition init_fen
setUpos (s:ss) = do
  let input = unwords (s:ss)
  let pinp0 = parse (symbol "startpos" >> symbol "moves" ) input
  let pinp1 = parse pGame input
  if null pinp0 && null pinp1 then mio $ errorCmd ["unknown command",
    input]
  else if null pinp0 then do
         let g = fst $ head pinp1
         let ns = snd $ head pinp1
         if null ns then do
           mSetPosition $ game2FEN g
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
setListMoves [m]    = mMakeMove m
setListMoves (m:ms) = do
  mMakeMove m
  setListMoves ms


uciLoop :: PlayArgs -> IO ()
uciLoop pa = do
             evalStateT uiLoop pa
