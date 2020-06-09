module Engine where

import Data.Maybe
import Moves
import Game
import Pieces(Side)

st_time = 5000::Int
st_depth = 10::Int
st_cp = False

type PlayArgs = (Int, Int, Bool, Game, [Game])

init_args :: PlayArgs
init_args = (st_time, st_depth, st_cp, initGame, [])

isInCheck :: Side -> Game -> Bool
isInCheck = undefined

makeMove :: Move -> Game -> Maybe Game
makeMove = undefined

think :: Game ->  Maybe Move
think = undefined
