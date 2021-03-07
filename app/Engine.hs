module Engine where

import Defs
import Data.Maybe (fromJust)
import Control.Monad.Trans.State
import Pieces (Side (..))
import Moves
import Game


-- vars
dft_time = 5000 :: Int
dft_depth = 10 :: Int
dft_post_flag = True
dft_str_move_w = "e2e4"
dft_str_move_b = "e7e5"
game_fen1 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
game_fen2 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
dft_cp_flag = Black
move_w = fromJust $ readMove "e2e4"
move_b = fromJust $ readMove "e7e5"
game_st1 = fromJust $ fen2Game game_fen1
game_st2 = fromJust $ fen2Game game_fen2
init_args = PlayArgs dft_time dft_depth dft_cp_flag init_game [] dft_post_flag


-- adts
data PlayArgs = PlayArgs {
  getTime :: Int
  ,getDepth :: Int
  ,getCpFlag :: Side
  ,getGame :: Game
  ,getHist :: [Game]
  ,getPost :: Bool
  } deriving (Eq,Show)


-- getters and setters
setTime :: Int -> PlayArgs -> PlayArgs
setTime a_time args = args{getTime=a_time}

setDepth :: Int -> PlayArgs -> PlayArgs
setDepth a_depth args = args{getDepth=a_depth}

setCpFlag :: Side -> PlayArgs -> PlayArgs
setCpFlag a_side args = args{getCpFlag=a_side}

setGame :: Game -> PlayArgs -> PlayArgs
setGame a_game args = args{getGame=a_game}

setHist :: [Game] -> PlayArgs -> PlayArgs
setHist a_hist args = args{getHist=a_hist}

setPost :: Bool -> PlayArgs -> PlayArgs
setPost a_post args = args{getPost=a_post}


-- main funcs
errorCmd :: [String] -> IO ()
errorCmd [] = putStrLn "Error (Incomplete)"
errorCmd [_] = putStrLn "Error (Incomplete)"
errorCmd  [a_type, a_msg] = putStrLn $ "Error (" ++ a_type ++  "): " ++ a_msg
errorCmd (r:cs) = errorCmd [r,head cs]


isInCheck :: Side -> Game -> Bool
isInCheck _ _ = False


makeMove :: Move -> Game -> Maybe Game
makeMove m g | (m == move_w) && (g == init_game) = Just game_st1
             | (m == move_b) && (g == game_st1) = Just game_st2
             | otherwise = Nothing


think :: Game -> Maybe Move
think g | g == init_game = Just move_w
        | g == game_st1 = Just move_b
        | otherwise = Nothing


takeBack :: StateT PlayArgs IO ()
takeBack = do
              args <- get
              let a_hist = getHist args
              let a_game = head a_hist
              let o_hist = drop 1 a_hist
              let args_ = setHist o_hist args
              let args__ = setGame a_game args_
              put args__


xMakeMove :: Move -> StateT PlayArgs IO ()
xMakeMove m = do
  args <- get
  let a_game = getGame args
  let n_game = makeMove m a_game
  maybe (mio $ putStrLn $ "ILLegal move: " ++ show m) (
    \g -> do
      let args_ = setGame g args
      put args_
           ) n_game


thinkMove :: StateT PlayArgs IO ()
thinkMove = do
  args <- get
  let a_game = getGame args
  let a_move = think a_game
  maybe adjudicate (
    \m -> do
      xMakeMove m
      mio $ putStrLn $ "move " ++ show m
    ) a_move


adjudicate :: StateT PlayArgs IO ()
adjudicate = do 
  args <- get
  let a_game = getGame args
  let a_side = turn a_game
  if isInCheck a_side a_game then do
    if a_side == White then do
      mio $ putStrLn "result 1-0 {White mates}"
    else do
      mio $ putStrLn "result 1-0 {Black mates}"
  else do
    mio $ putStrLn "result 1/2-1/2 {Stalemate}"

