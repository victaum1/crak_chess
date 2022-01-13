module Xboard where

-- import System.Console.Readline
import Data.Either
import Data.Maybe
import Control.Monad.Trans.State
import Defs
import Parsing
import Pieces
import Moves
import Game
import SubEngine
import Engine
import Utils


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
           ("xboard"   , const endOfLine )
           ,("otim"    , const endOfLine  )
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
           ,("dump", const mDump)
           ,("dumpfen", const mDumpFEN)
           ,("dumpplay", const mDumpPlay)
           ,("random", const endOfLine )
           ,("level", const endOfLine )
           ,("hard", const endOfLine )
           ,("easy", const endOfLine )
           ,("computer", const endOfLine )
           ,("white", const $ xTurn True)
           ,("black", const $ xTurn False)
         ]


-- funcs
new = do
      args <- get
      let args_ = init_args{getSeed=getSeed args}
      put args_


-- inner xboard loop
xTurn :: Bool -> StateT PlayArgs IO ()
xTurn f = do
          args <- get
          let arg_ = args{getCpFlag= Just f}
          put arg_


comXboard :: String -> StateT PlayArgs IO ()
comXboard line = do
--  mio $ print "comXboard..."
  if null line then endOfLine
    else do
      let input = words line
      let cmd = head input
      let args = tail input
      let res = lookup cmd xb_map
      let pm = parse pMoveCoord "" cmd
      if isLeft pm then maybe (mio (errorCmd ["unknown command",

                                            unwords input]))
        (\a -> a args) res else userMove [cmd]


xGo = do
  args <- get
  let g = getGame args
  let s = turn g
  let arg_ = args{getCpFlag=Just s}
  put arg_


setXpos :: [String] -> StateT PlayArgs IO ()
setXpos args | null args = mio (errorCmd ["incomplete", unwords args])
             | otherwise = do
                mSetPosition $ unwords args

accepted = endOfLine
rejected = endOfLine


ping :: [String] -> StateT PlayArgs IO ()
ping args | null args = mio (errorCmd ["incomplete", unwords args])
          | otherwise = do
              let n = parse pNat (head $ take 1 args) ""
              if isLeft n then
                mio (errorCmd ["not a number", unwords args])
              else
                mio $ putStrLn $ "pong " ++ show (myRight n)


protover :: [String] -> StateT PlayArgs IO ()
protover strs | null strs = mio (errorCmd ["incomplete",unwords strs])
              | otherwise = do
  let res_n = parse pNat (head $ take 1 strs) ""
  if isLeft res_n then mio (errorCmd ["incomplete",unwords strs])
    else do
           let n = myRight res_n
           if n==1 then endOfLine
             else if n==2 then do
               mio $ putStrLn features_str
               mio $ putStrLn "feature done=1"
             else endOfLine


userMove :: [String] -> StateT PlayArgs IO ()
userMove strs | null strs = mio (errorCmd ["incomplete",unwords strs])
              | otherwise =  do
  let res_m = parse pMoveCoord "" (head $ take 1 strs)
  if isLeft res_m then
    mio $ putStrLn $ "Illegal move (not a move): " ++ head strs
  else do
         let m = myRight res_m
         mMakeMove m
         args <- get
         if getCpFlag args == Just (turn (getGame args)) then
           xGo
         else endOfLine


undo = do
         args <- get
         let a_hist = getHist args
         if  null a_hist then endOfLine
           else do
                  mTakeBack


remove = do
           args <- get
           let a_hist = getHist args
           if length a_hist < 2 then endOfLine
             else do
                    mTakeBack
                    mTakeBack


force  :: StateT PlayArgs IO ()
force  = do
         args <- get
         let arg_ = args{getCpFlag=Nothing}
         put arg_


hint :: StateT PlayArgs IO ()
hint = do
          mio $ putStrLn "Hint: 0000"


post = do
          args <- get
          let args_ = setPost True args
          put args_


noPost = do
          args <- get
          let args_ = setPost False args
          put args_
