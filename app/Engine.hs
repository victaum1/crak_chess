module Engine where

import Defs
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Pieces (Side (..))
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
init_args = PlayArgs dft_time dft_depth dft_cp_flag init_game [] dft_post_flag


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
  let initsq = getInitSq m
  let desq   = getDestSq m
  initpiece <- checkSquare initsq b
  let o_pos = (desq,initpiece)
  let i_pos = (initsq,initpiece)
  let a_bd = Set.delete i_pos b
  let destpiece = checkSquare desq b
  if isNothing destpiece then return (Set.insert o_pos a_bd)
  else if (pieceSide <$> destpiece) == Just (pieceSide initpiece) then Nothing
    else return (Set.insert o_pos a_bd)


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
  let a_hist = getHist args
  let a_game = getGame args
  let n_game = makeMove m a_game
  maybe (mio $ putStrLn $ "ILLegal move: " ++ show m) (
    \g -> do
      let o_hist = a_game:a_hist
      let args_ = args{ getGame = g, getHist = o_hist}
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
      mio $ putStrLn "result 0-1 {Black mates}"
  else do
    mio $ putStrLn "result 1/2-1/2 {Stalemate}"


setPosition :: String -> StateT PlayArgs IO ()
setPosition strs = do
  pargs <- get
  let a_pos = fen2Game strs
  maybe (mio $ errorCmd ["not a FEN position!", strs])
    (\res -> do
        let o_args = setGame res pargs
        put o_args
    ) a_pos


dump :: StateT PlayArgs IO ()
dump = do
  args <- get
  let a_game = getGame args
  mio $ putStrLn $ showBoard $ board a_game

-- for debugging
dumpFEN :: StateT PlayArgs IO ()
dumpFEN = do
  args <- get
  let a_game = getGame args
  mio $ putStrLn $ "... " ++ unwords (drop 1 $ words $ game2FEN a_game)

dumpPlayArgs :: StateT PlayArgs IO ()
dumpPlayArgs = do
  args <- get
  let a_time = getTime args
  let a_depth = getDepth args
  let cp_flag = getCpFlag args
  let a_hist = getHist args
  let a_post = getPost args
  mio $ putStrLn $ "... Time: " ++ show a_time ++ ", Depth: " ++ show a_depth
    ++ ", CpFlat: " ++ show cp_flag ++ ", Hist: " ++ show (length a_hist)
    ++ ", Post: " ++ show a_post
