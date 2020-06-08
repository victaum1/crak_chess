module Main where

import System.IO
import System.Console.Readline
import Adapter
import Game
import Parsing
import Board
import Pieces(Side(..))
import Data.Maybe
import Engine
import Moves

version = "0.0.11.0"
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
play_map :: [ ( String, Play_Args -> IO ())]
play_map = [
  ("play", play_loop)
  ,("stop", stop)
  ,("new", new)
  ,("undo", undo)
  ,("dump", dump)
  ,("help", help_play)
  ,("quit", quit_play)
  ,("xboard", xboard_play)
  ,("uci", uci_play)
  ]

help_play args = do
  putStr help_str 
  play_loop args

quit_play _ = do
  quit

xboard_play _ = do
  xboard_loop

uci_play args = do
  uci_loop

play :: Move -> Play_Args -> IO ()
play move args = do
  let (st, sd, cp, game, history) = args
  if cp then do
    let move = think game
    if isNothing move then adjudicate game
    else do
      let _game  = makeMove (fromJust move) game
      let _history = (game : history)
      putStrLn $ show move
      let __game = fromJust _game
      let _args = (st, sd, False, __game, _history)
      play_loop _args 
  else do
    let _game = makeMove move game
    if isNothing _game then
      show_error move "Ilegal move: "
    else do
      let (Just a_game) = _game
      let a_history = (game : history)
      play_loop (st, sd, True, a_game, a_history)

stop args = do
  let (st, sd, cp, game, history) = args 
  let a_cp = False
  play_loop (st, sd, a_cp, game, history)

new  _ = play_loop init_args

undo :: Play_Args -> IO ()
undo args = do
  let (st, sd, cp, game, history) = args
  if null history then 
    do
       new args
  else
    let _game = head history
        _history = drop 1 history
        _args = (st, sd, cp,  _game, _history)
    in  
      play_loop _args

play_loop :: Play_Args -> IO ()
play_loop args = do
  res <- getPlay "play"
  if isNothing res then do
    quit
  else do
    let (Just _res) = res 
    let action = lookup _res play_map
    if isNothing action then do
      validate res args
    else do
      let (Just _action) = action
      _action args

validate line args = do
  case line of
    Nothing -> return ()
    Just inp -> do
      let move = parse pMoveCoord inp
      if null move then do
        show_error inp "(not a command, not a move): "
        play_loop args
      else do
        let [( _move, _ )] = move
        let (Just __move) = _move
        play __move args

show_error :: Show a => a -> String -> IO ()
show_error inp msg = do
  putStrLn $ msg ++ (show inp)

dump args = do
  let (_, _, _, game, _) = args 
  putStrLn $ showBoard $ getBoard game
  play_loop args

main_map :: [ (String, IO ()) ]
main_map = [
  ("quit", quit)
  ,("help", help_main)
  ,("xboard", xboard_loop)
  ,("uci", uci_loop)
  ,("play", main_play)
  ]

quit = do
  return ()

help_main = do
  putStr help_str
  main_loop

xboard = do
   xboard_loop 

uci = do
  uci_loop

main_play = do
  play_loop init_args


getPlay caller = readline $ caller ++ "> "

adjudicate game = do
  let side = getTurn game
  if is_in_check side game then
    if side == Black then
     putStrLn "White wins: {1-0}"
    else
     putStrLn "Black wins: {0-1}"
  else
    putStrLn "A Draw: { 1/2 - 1/2}"
  main_loop

main_loop :: IO ()
main_loop = do
   res <- getPlay "Crak"
   if isNothing res then do
     quit
   else do
     let (Just _res) = res
     let mbAction = lookup _res main_map
     if isNothing mbAction then do
       putStrLn ""
       main_loop
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
    main_loop 
