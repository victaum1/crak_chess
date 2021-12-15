module Xboard where

-- import System.Console.Readline
import Data.Maybe
import Control.Monad.Trans.State
import Defs
import Parsing
import Pieces
import Moves
import Game
import SubEngine
import Engine


-- vars
features_str = unwords [
  "feature"
  ,"usermove=1"
  ,"myname=" ++ name ++ version
  ,"colors=1"
  ,"ping=1"
  ,"setboard=1"
                       ]

xb_map :: [(String, [String] -> StateT PlayArgs IO ())]
xb_map = [
           ("xboard"   , const xbLoop)
           ,("otim"    , const xbLoop)
           ,("quit"    , const $ mio quit)
           ,("new"     , const new)
           ,("force"   , const force)
           ,("go"      , const xGo)
           ,("hint"    , const hint)
           ,("undo"    , const undo)
           ,("remove"  , const remove)
           ,("post"    , const post)
           ,("nopost"  , const noPost)
           ,("ping" , ping)
           ,("protover", protover)
           ,("usermove", userMove)
           ,("move", userMove)
           ,("accepted", const accepted)
           ,("rejected", const rejected)
           ,("setboard", setXpos)
           ,("dump", const (mDump >> xbLoop))
           ,("dumpfen", const (mDumpFEN >> xbLoop))
           ,("dumpplay", const (mDumpPlay >> xbLoop))
           ,("random", const xbLoop)
           ,("level", const xbLoop)
           ,("hard", const xbLoop)
           ,("easy", const xbLoop)
           ,("computer", const xbLoop)
           ,("white", const $ xTurn True)
           ,("black", const $ xTurn False)
         ]


-- funcs
new = do
      args <- get
      let args_ = init_args{getSeed=getSeed args}
      put args_
      xbLoop

-- inner xboard loop
xTurn :: Bool -> StateT PlayArgs IO ()
xTurn f = do
          args <- get
          let arg_ = args{getCpFlag= Just f}
          put arg_
          xbLoop


xbLoop :: StateT PlayArgs IO ()
xbLoop = do
  line <- mio getLine
  if null line then xbLoop
    else do
      let input = words line
      let cmd = head input
      let args = tail input
      let res = lookup cmd xb_map
      let pm = parse pMoveCoord cmd
      if null pm then maybe (mio (errorCmd ["unknown command",
                                            unwords input]) >>
                              xbLoop)
        (\a -> a args) res else userMove [cmd]
  

xGo = xThink >> xbLoop

xThink :: StateT PlayArgs IO ()
xThink = do
          args <- get
          if getPost args then printPost >> mThinkMove
          else mThinkMove


setXpos :: [String] -> StateT PlayArgs IO ()
setXpos args | null args = mio (errorCmd ["incomplete", unwords args]) >>
               xbLoop
             | otherwise = do
                mSetPosition $ unwords args
                xbLoop

accepted = xbLoop
rejected = xbLoop


ping :: [String] -> StateT PlayArgs IO ()
ping args | null args = mio (errorCmd ["incomplete", unwords args]) >> xbLoop
          | otherwise = do
              let n = parse nat $ head $ take 1 args
              if isNothing n then mio (errorCmd ["not a number", unwords args])
                >>
                xbLoop
              else do
                mio $ putStrLn $ "pong " ++ show (fromJust $ fst <$> n)
                xbLoop


protover :: [String] -> StateT PlayArgs IO ()
protover strs | null strs = mio (errorCmd ["incomplete",unwords strs]) >> xbLoop
              | otherwise = do
  let res_n = parse nat $ head $ take 1 strs
  if isNothing res_n then mio (errorCmd ["incomplete",unwords strs]) >> xbLoop
    else do
           let n = fromJust $ fst <$> res_n
           if n==1 then xbLoop
             else if n==2 then do
               mio $ putStrLn features_str
               mio $ putStrLn "feature done=1"
               xbLoop
             else xbLoop


userMove :: [String] -> StateT PlayArgs IO ()
userMove strs | null strs = mio (errorCmd ["incomplete",unwords strs]) >> xbLoop
              | otherwise =  do
  let res_m = parse pMoveCoord $ head $ take 1 strs
  if isNothing res_m then do
    mio $ putStrLn $ "Illegal move (not a move): " ++ head strs
    xbLoop
  else do
         let m = fromJust $ fst <$> res_m
         mMakeMove m
         args <- get
         if getCpFlag args == Just (turn (getGame args)) then
           xGo
         else xbLoop


undo = do
         args <- get
         let a_hist = getHist args
         if  null a_hist then xbLoop
           else do
                  mTakeBack
                  xbLoop


remove = do
           args <- get
           let a_hist = getHist args
           if length a_hist < 2 then xbLoop
             else do
                    mTakeBack
                    mTakeBack
                    xbLoop


force = do
          force'
          xbLoop




force' :: StateT PlayArgs IO ()
force' = do
         args <- get
         let arg_ = args{getCpFlag=Nothing}
         put arg_


printPost :: StateT PlayArgs IO ()
printPost = do
          args <- get
          if turn (getGame args) then
              mio $ putStrLn "1 0 1 1 e2e4"
          else mio $ putStrLn "1 0 1 1 e7e5"


hint :: StateT PlayArgs IO ()
hint = do
          args <- get
          if turn (getGame args) then mio $ putStrLn "Hint: e2e4"
            else mio $ putStrLn "Hint: e7e5"
          xbLoop


post = do
          args <- get
          let args_ = setPost True args
          put args_
          xbLoop


noPost = do
          args <- get
          let args_ = setPost False args
          put args_
          xbLoop



-- main loop
xboardLoop :: PlayArgs -> IO ()
xboardLoop pa = do
  putStrLn ""
  evalStateT xbLoop pa

