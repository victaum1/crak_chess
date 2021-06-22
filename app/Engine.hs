module Engine where

import Data.Maybe (fromJust, isNothing, isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State
import Moves
import Game
import Board
import Pieces

-- vars / cons
dft_time = 5000 :: Int
dft_depth = 10 :: Int
dft_post_flag = True
dft_str_move_w = "e2e4"
dft_str_move_b = "e7e5"
game_fen1 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
game_fen2 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
dft_cp_flag = Black
move_w = fromJust $ readMove dft_str_move_w
move_b = fromJust $ readMove dft_str_move_b
game_st1 = fromJust $ fen2Game game_fen1
game_st2 = fromJust $ fen2Game game_fen2
init_args = PlayArgs dft_time dft_depth dft_cp_flag init_game []
  dft_post_flag


-- adts
data PlayArgs = PlayArgs {
   getTime   :: Int
  ,getDepth  :: Int
  ,getCpFlag :: Side
  ,getGame   :: Game
  ,getHist   :: [Game]
  ,getPost   :: Bool
  } deriving (Eq,Show)


-- setters for PlayArgs
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
isInCheck :: Side -> Game -> Bool
isInCheck _ _ = False


makeMove :: Move -> Game -> Maybe Game
makeMove m g = do
  let i_side = turn g
  let i_nplys = nPlys g
  let i_nMoves = nMoves g
  let o_nplys = if isPawn || isCapture then 0 else i_nplys + 1
  let o_nMoves = if i_side == Black then i_nMoves + 1 else i_nMoves
  o_bd <- mkMoveBoard m i_bd
  return (g{board=o_bd, turn = revertSide i_side, nPlys = o_nplys
           , nMoves = o_nMoves})
  where revertSide s = if s == White then Black else White
        isPawn = Just Pawn == (pieceType <$> checkSquare (getInitSq m) i_bd)
        i_bd = board g
        isCapture = isJust $ checkSquare (getDestSq m) i_bd

mkMoveBoard :: Move -> Board -> Maybe Board
mkMoveBoard m b = do
  let initsq    = getInitSq m
  let desq      = getDestSq m
  initpiece <- checkSquare initsq b
  let a_bd      = Map.delete initsq b
  let destpiece = checkSquare desq b
  if isNothing destpiece then return
    (Map.insert desq initpiece a_bd)
  else if (pieceSide <$> destpiece) == (pieceSide <$>
    Just initpiece) then Nothing
  else return (Map.insert desq (fromJust destpiece) a_bd)


think :: Game -> Maybe Move
think g | g == init_game = Just move_w
        | g == game_st1 = Just move_b
        | otherwise = Nothing


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
