module Main where

import           Adapter
import           Board
import           Data.Maybe
import           Engine
import           Game
import           Moves
import           Parsing
import           Pieces                  (Side (..))
import           System.Console.Readline
import           System.IO

version = "0.0.13.8"
help_str = unlines [
  "play  - Engine thinks and plays for the current turn."
  ,"stop - Engine Stops (Human vs Human)."
  ,"st n - Sets the time per move in seconds."
  ,"sd n - Sets the deph of searching."
  ,"undo - Undo a move."
  ,"new - New game."
  ,"dump - Dumps the board."
  ,"quit - Exits the engine."
  ,"xboard - Switch to xboard protocol."
  ,"uci - Switch to uci protocol."
  ,"Enter moves in coordinate notation. Eg: 'e2e4', 'a7a8Q'"
  ]

play_map :: [ ( String, PlayArgs -> IO ())]
play_map = [
  ("play", playLoop)
  ,("stop", stop)
  ,("new", new)
  ,("undo", undo)
  ,("dump", dump)
  ,("help", helpPlay)
  ,("quit", quitPlay)
  ,("xboard", xboardPlay)
  ,("uci", uciPlay)
  ]

helpPlay args = do
  putStr help_str
  playLoop args

quitPlay _ = quit

xboardPlay _ = xboardLoop

uciPlay args = uciLoop

play :: Move -> PlayArgs -> IO ()
play move args = do
  let (st, sd, cp, game, history) = args
  if cp then do
    let move = think game
    if isNothing move then adjudicate game
    else do
      let _game  = makeMove (fromJust move) game
      let _history = game : history
      print (maybe " " showMove move)
      let __game = fromJust _game
      let _args = (st, sd, False, __game, _history)
      playLoop _args
  else do
    let _game = makeMove move game
    if isNothing _game then do
      showMsg (showMove move) "Ilegal move: "
      playLoop args
    else do
      let (Just a_game) = _game
      let a_history = game : history
      playLoop (st, sd, True, a_game, a_history)

stop args = do
  let (st, sd, _, game, history) = args
  let a_cp = False
  playLoop (st, sd, a_cp, game, history)

new  _ = playLoop init_args

undo :: PlayArgs -> IO ()
undo args = do
  let (st, sd, cp, _, history) = args
  if null history then new args
  else
    let _game = head history
        _history = drop 1 history
        _args = (st, sd, cp,  _game, _history)
    in
      playLoop _args

playLoop :: PlayArgs -> IO ()
playLoop args = do
  res <- getPlay "playing"
  if isNothing res then quit
  else do
    let (Just _res) = res
    let action = lookup _res play_map
    if isNothing action then validate res args
    else do
      let (Just _action) = action
      _action args

validate :: Maybe String -> PlayArgs -> IO ()
validate line args =
  case line of
    Nothing -> return ()
    Just inp -> do
      let move = parse pMoveCoord inp
      if null move then do
        showMsg inp "Error (not a command, not a move): "
        playLoop args
      else do
        let [( _move, _ )] = move
        let (Just __move) = _move
        play __move args

showMsg :: Show a => a -> String -> IO ()
showMsg inp msg = putStrLn $ msg ++ show inp

dump :: PlayArgs -> IO ()
dump args = do
  let (_, _, _, game, _) = args
  putStrLn $ showBoard $ getBoard game
  playLoop args

main_map :: [ (String, IO ()) ]
main_map = [
  ("quit", quit)
  ,("help", mainHelp)
  ,("xboard", xboardLoop)
  ,("uci", uciLoop)
  ,("play", mainPlay)
  ]

quit :: IO ()
quit = return ()

mainHelp = do
  putStr help_str
  mainLoop

xboard = xboardLoop

uci = uciLoop

mainPlay = playLoop init_args


getPlay caller = readline $ caller ++ "> "

adjudicate game = do
  let side = getTurn game
  if isInCheck side game then
    if side == Black then
     putStrLn "White wins: {1-0}"
    else
     putStrLn "Black wins: {0-1}"
  else
    putStrLn "A Draw: { 1/2 - 1/2}"
  mainLoop

mainLoop :: IO ()
mainLoop = do
   res <- getPlay "Idle"
   if isNothing res then quit
   else do
     let (Just _res) = res
     let mbAction = lookup _res main_map
     if isNothing mbAction then do
       putStrLn ""
       mainLoop
     else do
       let (Just action) = mbAction
       action

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Craken " ++ version ++ " by V. Manotas."
    putStrLn "x/x/2020."
    putStrLn ""
    putStrLn "'help' show usage."
    mainLoop
