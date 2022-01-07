{-# LANGUAGE BangPatterns #-}
module Engine where

-- import Data.Either ()
-- import Data.Maybe ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random ( StdGen )
import System.Clock (Clock (Monotonic, Realtime), getTime, TimeSpec(..), timeSpecAsNanoSecs, diffTimeSpec, toNanoSecs)
import Moves ( readMove, Move )
import Game ( board, fen2Game, game2FEN, init_game, Game, nMoves )
import Board ( showBoard )
import Pieces ( Side )
import Valid ( genValidMoves )
import Defs ( randomChoice, mio )
import Utils ( myRight )
import Search ( searchDivide, MoveScore, Depth, Nodes, SearchInfo )
import Control.Monad.Trans.State ( get, put, StateT )
import Evaluate (Score)

-- vars / cons
max_depth = 22 :: Int
dft_time = 300000 :: Int -- ms, 5 mins
dft_post_flag = True
dft_cp_flag = Just False -- Black
dft_seed = Nothing -- Auto gen random number
dft_protocol = True -- Xboard protocol || UCI prot...

init_args = PlayArgs dft_time max_depth dft_cp_flag init_game []
  dft_post_flag dft_seed dft_protocol

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
  ,getProtocol :: Bool
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
think :: StateT PlayArgs IO (Maybe Move)
think = do
  args <- get
  let ttime = getCTime args
  ti <- mio $ getTime Realtime
  ms <- iterDeep ti 1
  tf <- mio $ getTime Realtime
  let dif = difTime tf ti
--  mio $ print dif
  put (setTime (ttime-(fromInteger dif `div` round 1e6)) args)
  if not (null ms) then return (Just (pickMove ms))
    else return Nothing

pickMove :: [SearchInfo] -> Move
pickMove ms = (\(a,_,_) -> a) $ last ms


difTime :: TimeSpec -> TimeSpec -> Integer -- nano secs
difTime tf ti = toNanoSecs $ diffTimeSpec tf ti


checkTime :: TimeSpec -> StateT PlayArgs IO Bool
checkTime ti = do
  tf <- mio $ getTime Realtime
  let dif = difTime tf ti
  args <- get
  let ttime = getCTime args
  mob <- movesOutOfBook
  let nMs = min mob 5
  let facTor = 2 - toRational nMs / 5
  mtc <- movesUntiTimeControl
  let target = (ttime * round 1e6) `div` (mtc + 5)
  let te = round (fromIntegral target * facTor)
--  mio $ print dif
  if 2*dif > te then return True
  else return False

movesUntiTimeControl :: StateT PlayArgs IO Int
movesUntiTimeControl = do
  args <- get
  let g = getGame args
  let nms = nMoves g
  if nms > 40 then return 0
  else return (40 - nms)


movesOutOfBook :: StateT PlayArgs IO Int
movesOutOfBook = do
  args <- get
  let g = getGame args
  let nms = nMoves g
  if nms < 5 then return 0
  else return (nms - 5)


postInfo :: Bool -> Protocol -> Depth -> Score -> Nodes -> Move -> IO ()
postInfo b p d s n m | b = if p then putStrLn $ show d ++ " " ++ show s ++ " "
                                ++ show n ++ " " ++ show m
                           else putStrLn $ "info depth " ++ show d ++
                                " score cp " ++ show s ++ " nodes " ++ show n
                                ++ " pv " ++ show m
                     | otherwise = putStr ""


iterDeep :: TimeSpec -> Depth -> StateT PlayArgs IO [SearchInfo]
iterDeep ti ni = do
  args <- get
  let g = getGame args
  let prot = getProtocol args
  let post = getPost args
  let !ms = searchDivide g ni
  tc <- checkTime ti
  let (m,s,_) = last ms
  let nodes = sum $ map (\(_,_,n) -> n) ms
  mio $ postInfo post prot ni s nodes m
  if ni < max_depth && not tc then iterDeep ti (ni+1)
      else return ms


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
  let a_prot = getProtocol pa
  "... Time: " ++ show a_time ++ ", Depth: " ++ show a_depth
    ++ ", CpFlat: " ++ show cp_flag ++ ", Hist: " ++ show (length a_hist)
    ++ ", Post: " ++ show a_post ++ ", Prot: " ++ show a_prot

