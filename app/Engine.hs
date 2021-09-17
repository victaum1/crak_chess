module Engine where

-- import Data.Maybe (fromJust, isNothing, isJust)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Control.Monad.Trans.State
-- import System.Random
-- import Moves
-- import Game
-- import Board
-- import Pieces
-- import Valid
-- import Defs
-- import Search

import Data.Maybe (fromJust, isNothing, isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State ()
import System.Random ( StdGen )
import Moves ( readMove, Move, null_move )
import Game
    ( fen2Game, game2FEN, init_game, Game, GameState(board) )
import Board ( showBoard )
import Pieces ( Side )
import Valid ( genValidMoves )
import Defs ( randomChoice )
import Search (MoveScore, searchList, search)
import Evaluate (Delta,Score) 

-- vars / cons
dft_time = 5000 :: Int
dft_depth = 10 :: Int
dft_post_flag = True
dft_str_move_w = "e2e4"
dft_str_move_b = "e7e5"
game_fen1 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
game_fen2 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
dft_cp_flag = Just False -- Black
dft_seed = Nothing -- Auto gen random number
move_w = fromJust $ readMove dft_str_move_w
move_b = fromJust $ readMove dft_str_move_b
game_st1 = fromJust $ fen2Game game_fen1
game_st2 = fromJust $ fen2Game game_fen2
init_args = PlayArgs dft_time dft_depth dft_cp_flag init_game []
  dft_post_flag dft_seed


-- adts
data PlayArgs = PlayArgs {
   getTime   :: Int
  ,getDepth  :: Int
  ,getCpFlag :: Maybe Side
  ,getGame   :: Game
  ,getHist   :: [Game]
  ,getPost   :: Bool
  ,getSeed   :: Maybe Int
  } deriving (Eq,Show)


-- setters for PlayArgs
setTime :: Int -> PlayArgs -> PlayArgs
setTime a_time args = args{getTime=a_time}

setDepth :: Int -> PlayArgs -> PlayArgs
setDepth a_depth args = args{getDepth=a_depth}

setCpFlag :: Maybe Side -> PlayArgs -> PlayArgs
setCpFlag a_side args = args{getCpFlag=a_side}

setGame :: Game -> PlayArgs -> PlayArgs
setGame a_game args = args{getGame=a_game}

setHist :: [Game] -> PlayArgs -> PlayArgs
setHist a_hist args = args{getHist=a_hist}

setPost :: Bool -> PlayArgs -> PlayArgs
setPost a_post args = args{getPost=a_post}


-- main funcs
pickMove :: StdGen -> Delta -> [MoveScore] -> Move
pickMove sg d ms | null ms = null_move
              | length ms == 1 = (fst . head) ms
              | otherwise = move
  where (_,top) = last ms
        (_,mt) = break bf ms
        bf = \(m,s) -> s >= top - d
        move = fst $ randomChoice sg mt

think :: StdGen -> Game -> Maybe Move
think sg gm | not (null ms) = Just move
            | otherwise = Nothing
  where msn = searchList 4 gm
        ms = map (\(a,b,_)->(a,b)) msn
        move = pickMove sg 5 ms


takeBack :: PlayArgs -> PlayArgs
takeBack iargs = setGame a_game args_
  where a_hist = getHist iargs
        a_game = head a_hist
        o_hist = drop 1 a_hist
        args_ = setHist o_hist iargs


dump :: PlayArgs -> String
dump pa = do
  let a_game = getGame pa
  showBoard $ board a_game


-- for debugging
dumpFEN :: PlayArgs -> String
dumpFEN pa = do
  let a_game = getGame pa
  "... " ++ unwords (drop 1 $ words $ game2FEN a_game)

dumpPlay :: PlayArgs -> String
dumpPlay pa = do
  let a_time = getTime pa
  let a_depth = getDepth pa
  let cp_flag = getCpFlag pa
  let a_hist = getHist pa
  let a_post = getPost pa
  "... Time: " ++ show a_time ++ ", Depth: " ++ show a_depth
    ++ ", CpFlat: " ++ show cp_flag ++ ", Hist: " ++ show (length a_hist)
    ++ ", Post: " ++ show a_post
