module Main where

import System.IO
import System.Console.Haskeline
import Adapter
import Game
import Parsing
import Board
import Pieces
import Data.Maybe
import Engine
import Moves

st_time = 5000::Int
st_depth = 10::Int
st_cp = False

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

play_cmds = [
  "play",
  "stop",
  "new",
  "undo",
  "dump"
  ]

main_cmds = [
  "quit"
  ,"help"
  ,"xboard"
  ,"uci"
  ]

type Args_Play = (Int, Int, Bool, Game, [Game])

init_args = (st_time, st_depth, st_cp, initGame, [])

getPlay caller = getInputLine $ caller ++ "> "

adjudicate game = do
  side <- return (getTurn game)
  if is_in_check side game then
    if side == Black then
     outputStrLn "White wins: {1-0}"
    else
     outputStrLn "Black wins: {0-1}"
  else
    outputStrLn "A Draw: { 1/2 - 1/2}"
  main_loop "" "crak" init_args

play_loop :: String -> String -> Args_Play -> InputT IO () 
play_loop res caller args  = do
  let (_st, _sd, _cp, _game, _history) = args 
  case res of
    "play" -> do
      if _cp then do
        _move <- return (think _game)
        if isNothing _move then adjudicate _game
        else do
          a_move <- return (fromJust _move)
          _game <- return (makeMove a_move _game)
          a_game <- return (fromJust _game)
          a_history <- return (a_game : _history)
          outputStrLn $ show a_move
          line <- getPlay "play"
          _args <- return (_st, _sd, False, a_game, a_history)
          validate_line line "play" _args
      else do
        a_cp <- return True
        line <- getPlay "play"
        validate_line line "play" (_st, _sd, True, _game, _history) 
    "stop" -> do
        a_cp <- return False
        line <- getPlay "play"
        validate_line line "play" (_st, _sd, a_cp, _game, _history) 
    "new" -> do
      init_play
    "dump" -> do
      outputStrLn $ showBoard $ getBoard _game
      line <- getPlay "play"
      validate_line line "play" args 
    "undo" -> do
      if null _history then 
        do
          play_loop "" "play" args
      else
        do
          _game <- return (head _history)
          _history <- return (drop 1 _history)
          _args <- return (_st, _sd, _cp,  _game, _history)  
          play_loop "" "play" _args 
    "" -> do
        line <- getPlay "play"
        validate_line line "play" args 
    _ -> do
      _move <- return (readMove res)
      if isNothing _move then do
        outputStrLn $ "Error (Invalid move): " ++ res
        line <- getPlay "play"
        validate_line line caller args
      else do
            a_move <- return (fromJust _move)
            _game <- return (makeMove a_move _game)
            if isNothing _game then do 
              show_error _game "Invalid move : "
              line <-getPlay "play"
              validate_line line "play" args
            else do
              a_game <- return (fromJust _game)
              a_history <- return (a_game : _history)
              line <- getPlay "play"
              _args <- return (_st, _sd, True, a_game, a_history)
              validate_line line "play" _args
  where normal_play = play_loop "" caller args  

init_play = play_loop "dump" "play" init_args  

show_error :: Show a => a -> String -> InputT IO ()
show_error inp msg = do
  outputStrLn $ msg ++ (show inp)

validate_line line caller play_tuple = do
  case line of
    Nothing -> return ()
    Just inp -> do
      if inp `elem` main_cmds then
        main_loop inp "crak" play_tuple
      else
        if inp `elem` play_cmds then
          play_loop inp "play" play_tuple
        else
          if null $ parse pMoveCoord inp then do
            show_error inp "Error (Unknown command): "
            line <- getPlay "play"
            validate_line line caller play_tuple
          else do
            play_loop inp "play" play_tuple

main_loop res caller play_tuple = do
  case res of
    "quit" -> return ()
    "xboard" -> xboard_loop
    "uci" -> uci_loop
    "help" -> do
      outputStrLn help_str
      line <- getPlay caller
      validate_line line caller play_tuple
    _ -> do
      line <- getPlay caller 
      validate_line line caller play_tuple
    
main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         runInputT defaultSettings entry
       where
         entry :: InputT IO ()
         entry = do
           outputStrLn $ "Craken " ++ version ++ " by V. Manotas."
           outputStrLn "x/x/2019."
           outputStrLn ""
           outputStrLn "'help' show usage."
           main_loop "" "crak" init_args
