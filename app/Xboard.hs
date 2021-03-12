module Xboard where

import Moves
import Parsing
import Defs
import Pieces
import Game
import Engine
-- import System.Console.Readline
import Control.Monad.Trans.State


-- vars
features_str = unwords [
  "feature"
  ,"usermove=1"
  ,"myname=\"Craken 0.0.13.8\""
  ,"colors=0"
  ,"ping=1"
  ,"setboard=1"
                       ]

xb_map :: [(String, [String] -> StateT PlayArgs IO ())]
xb_map = [
           ("xboard"   , const xbLoop)
           ,("otim"    , const xbLoop)
           ,("quit"    , const $ mio quit)
           ,("new"     , const $ mio xboardLoop)
           ,("force"   , const force)
           ,("go"      , const go)
           ,("hint"    , const hint)
           ,("undo"    , const undo)
           ,("remove"  , const remove)
           ,("post"    , const post)
           ,("nopost"  , const noPost)
           ,("ping" , ping)
           ,("protover", protover)
           ,("usermove", userMove)
           ,("accepted", const accepted)
           ,("rejected", const rejected)
           ,("setboard", setXpos)
           ,("dump", const (dump >> xbLoop))
           ,("dumpfen", const (dumpFEN >> xbLoop))
           ,("dumpplay", const (dumpPlayArgs >> xbLoop))
         ]


-- funcs
-- inner xboard loop
xbLoop :: StateT PlayArgs IO ()
xbLoop = do
  line <- mio getLine
  if null line then xbLoop
    else do
      let input = words line
      let cmd = head input
      let args = tail input
      let res = lookup cmd xb_map
      maybe (mio (errorCmd ["unknown command", unwords input]) >> xbLoop)
        (\a -> a args) res


setXpos :: [String] -> StateT PlayArgs IO ()
setXpos args | null args = mio (errorCmd ["incomplete", unwords args]) >> xbLoop
             | otherwise = do
                setPosition $ unwords args
                xbLoop

accepted = xbLoop
rejected = xbLoop


ping :: [String] -> StateT PlayArgs IO ()
ping args | null args = mio (errorCmd ["incomplete", unwords args]) >> xbLoop
          | otherwise = do
              let n = parse nat $ head $ take 1 args
              if null n then mio (errorCmd ["not a number", unwords args]) >> xbLoop
                else do
                  mio $ putStrLn $ "pong " ++ show (fst $ head n)
                  xbLoop


protover :: [String] -> StateT PlayArgs IO ()
protover strs | null strs = mio (errorCmd ["incomplete",unwords strs]) >> xbLoop
              | otherwise = do
  let res_n = parse nat $ head $ take 1 strs
  if null res_n then mio (errorCmd ["incomplete",unwords strs]) >> xbLoop
    else do
           let n = fst $ head res_n
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
  if null res_m then do
    mio $ putStrLn $ "Illegal move (not a move): " ++ head strs
    xbLoop
  else do
         let m = fst $ head res_m
         xMakeMove m
         go


undo = do
         args <- get
         let a_hist = getHist args
         if  null a_hist then xbLoop
           else do
                  takeBack
                  xbLoop


remove = do
           args <- get
           let a_hist = getHist args
           if length a_hist < 2 then xbLoop
             else do
                    takeBack
                    takeBack
                    xbLoop


force = do
          force'
          xbLoop


go = do
        force'
        xThink


force' :: StateT PlayArgs IO ()
force' = do
          args <- get
          let a_side = turn $ getGame args
          let args_ = setCpFlag a_side args
          put args_


xThink :: StateT PlayArgs IO ()
xThink = do
          args <- get
          if getPost args then do
            printPost
            thinkMove
            xbLoop
          else thinkMove >> xbLoop


printPost = do
          args <- get
          if turn (getGame args) == White then
              mio $ putStrLn "1 0 1 1 e2e4"
          else mio $ putStrLn "1 0 1 1 e7e5"


hint :: StateT PlayArgs IO ()
hint = do
          args <- get
          if turn (getGame args) == White then mio $ putStrLn "Hint: e2e4"
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
xboardLoop :: IO ()
xboardLoop = do
  putStrLn ""
  evalStateT xbLoop init_args

