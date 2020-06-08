module Engine where

import Data.Maybe
import Moves
import Game
import Pieces(Side)

st_time = 5000::Int
st_depth = 10::Int
st_cp = False

type Play_Args = (Int, Int, Bool, Game, [Game])

init_args :: Play_Args
init_args = (st_time, st_depth, st_cp, initGame, [])

is_in_check :: Side -> Game -> Bool
is_in_check = undefined

makeMove :: Move -> Game -> Maybe Game
makeMove = undefined

think :: Game ->  Maybe Move
think = undefined
