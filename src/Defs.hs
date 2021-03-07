module Defs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

version = "0.0.13.8"

quit :: IO ()
quit = return ()

mio :: IO a -> StateT s IO a
mio = lift

