module Defs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random

version = "0.1.9.0"
author  = "V. Manotas"
name    = "Craken"
date    = "06/01/2022"

randomChoice :: [a] -> StdGen -> a
randomChoice l g = l !! fst (randomR (0, length l - 1) g)

-- IO
quit :: IO ()
quit = return ()

mio :: IO a -> StateT s IO a
mio = lift


endOfLine = mio $ putStr ""


errorCmd :: [String] -> IO ()
errorCmd [] = putStrLn "Error (Incomplete)"
errorCmd [_] = putStrLn "Error (Incomplete)"
errorCmd  [a_type, a_msg] = putStrLn $ "Error (" ++ a_type ++  "): "
   ++ a_msg
errorCmd (r:cs) = errorCmd [r,head cs]
