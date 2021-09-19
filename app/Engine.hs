{-# LANGUAGE BangPatterns #-}
module Engine where

-- import System.Random
-- import Control.Monad.Trans.State
-- import System.Clock
-- import Data.Maybe
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Moves
-- import Game
-- import Board
-- import Pieces
-- import Valid
-- import Defs
-- import Search
-- import Evaluate


import System.Random ( mkStdGen, newStdGen, StdGen )
import Control.Monad.Trans.State ( get, StateT )
import System.Clock
    ( getTime, Clock(Monotonic), TimeSpec(TimeSpec) )
import Data.Maybe ( isNothing )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moves ( null_move, Move )
import Game ( game2FEN, init_game, Game, GameState(board) )
import Board ( showBoard )
import Pieces ( Side )
import Valid ()
import Defs ( mio, randomChoice )
import Search ( searchList, Depth, MoveInfo, MoveScore, Nodes )
import Evaluate ( Score, Delta )

-- vars / cons
max_depth = 60 :: Int
dft_time = 300 :: Int -- seconds
dft_post_flag = True
dft_cp_flag = Just False -- Black
dft_seed = Nothing -- Auto gen random number
init_args = PlayArgs dft_time max_depth dft_cp_flag init_game []
  dft_post_flag dft_seed True


-- simple types
type Protocol = Bool

-- adts
data PlayArgs = PlayArgs {
   getCTime   :: Int
  ,getDepth  :: Int
  ,getCpFlag :: Maybe Side
  ,getGame   :: Game
  ,getHist   :: [Game]
  ,getPost   :: Bool
  ,getSeed   :: Maybe Int
  ,getProt   :: Bool
  } deriving (Eq,Show)


-- setters for PlayArgs
setTime :: Int -> PlayArgs -> PlayArgs
setTime a_time args = args{getCTime=a_time}

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

-- think :: StdGen -> Game -> Maybe Move
-- think sg gm | not (null ms) = Just move
--            | otherwise = Nothing
--  where msn = searchList 4 gm
--        ms = map (\(a,b,_)->(a,b)) msn
--        move = pickMove sg 5 ms

think :: StateT PlayArgs IO (Maybe Move)
think = do
  args <- get
  gen <- newStdGen
  let sg = maybe gen mkStdGen (getSeed args)
  ti <- mio $ getTime Monotonic
  msn <- iterDeep ti 1
  let ms = map (\(a,b,_)->(a,b)) msn
  if not (null ms) then return (Just (pickMove sg 10 ms))
  else return Nothing


checkStop :: StateT PlayArgs IO Bool
checkStop = do
  args <- get
  let cpf = getCpFlag args
  if  isNothing cpf then return True
    else return False


postInfo :: Protocol -> Depth -> Score -> Nodes -> Move -> IO ()
postInfo p d s n m | p = putStrLn $ show d ++ " " ++ show s ++ " "
                   ++ show n ++ " " ++ show m
                 | otherwise = putStrLn $ "info depth " ++ show d ++
                   " score cp " ++ show s ++ " nodes " ++ show n
                   ++ " pv " ++ show m


difTime :: TimeSpec -> TimeSpec -> Int
difTime (TimeSpec fs fns) (TimeSpec is ins)  = fromIntegral
  $ (fs - is) * round 1e9 + (fns - ins)


checkTime :: TimeSpec -> StateT PlayArgs IO Bool
checkTime ti = do
  to <- mio $ getTime Monotonic
  let dif = difTime to ti
  args <- get
  let ttime = getCTime args
  if dif >= (ttime * round 1e9) `div` 43
    then return True else return False

iterDeep :: TimeSpec -> Depth -> StateT PlayArgs IO [MoveInfo]
iterDeep ti ni = do
  args <- get
  let g = getGame args
  let post = getPost args
  let prot = getProt args
  let !msn = searchList ni g
  b <- checkStop
  tc <- checkTime ti
  let (m,sc,_) = last msn
  let nds = map (\(_,_,a) -> a) msn
  let nd = sum nds
  if not tc && not b && ni < max_depth then do
    if post then do
      mio $ postInfo prot ni sc nd m
      iterDeep ti (ni+1)
    else iterDeep ti (ni+1)
  else
     return msn


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
  let a_time = getCTime pa
  let a_depth = getDepth pa
  let cp_flag = getCpFlag pa
  let a_hist = getHist pa
  let a_post = getPost pa
  let a_prot = getProt pa
  "... Time: " ++ show a_time ++ ", Depth: " ++ show a_depth
    ++ ", CpFlat: " ++ show cp_flag ++ ", Hist: " ++ show (length a_hist)
    ++ ", Post: " ++ show a_post ++ ", Prot: " ++ show a_prot
