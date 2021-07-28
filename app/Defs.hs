module Defs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

version = "0.0.13.8"

-- IO
quit :: IO ()
quit = return ()

mio :: IO a -> StateT s IO a
mio = lift

errorCmd :: [String] -> IO ()
errorCmd [] = putStrLn "Error (Incomplete)"
errorCmd [_] = putStrLn "Error (Incomplete)"
errorCmd  [a_type, a_msg] = putStrLn $ "Error (" ++ a_type ++  "): "
   ++ a_msg
errorCmd (r:cs) = errorCmd [r,head cs]
