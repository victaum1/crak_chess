module IoSearch where

import Engine (PlayArgs (getProtocol), getCpFlag, getGame, think)
import SubEngine (mThinkMove)
import Defs (mio, endOfLine)
import Control.Monad.Trans.State (StateT, get, put)
import Game (turn)
import Data.Maybe (isNothing)


ioSearch :: StateT PlayArgs IO ()
ioSearch = do
  pa <-get
  let cpf = getCpFlag pa
  let pt  = getProtocol pa
  let s = turn $ getGame pa
  if isNothing cpf then endOfLine else do
    if cpf == Just s && pt then mThinkMove
    else do
      a_move <- think
      maybe (mio $ putStrLn "bestmove 0000") ( -- ilegal?
         \m -> do
         mio $ putStrLn $ "bestmove " ++ show m) a_move
