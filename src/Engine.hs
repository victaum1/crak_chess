module Engine where

import Data.Maybe
import Moves
import Game
import Pieces

is_in_check :: Side -> Game -> Bool
is_in_check = undefined

makeMove :: Move -> Game -> Maybe Game
makeMove = undefined

think :: Game ->  Maybe Move
think = undefined
