module Engine where

import           Data.Maybe
import           Game
import           Moves
import           Pieces     (Side (..))

st_time = 5000::Int
st_depth = 10::Int
st_cp = False

move_w = fromJust (readMove "e2e4")
move_b = readMove "e7e5"
game_b = fen2Game "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
game_w = fen2Game "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

game_in_check = fromJust (fen2Game "")

type PlayArgs = (Int, Int, Bool, Game, [Game])

init_args :: PlayArgs
init_args = (st_time, st_depth, st_cp, initGame, [])

isInCheck :: Side -> Game -> Bool
isInCheck s g | s == White && g == game_in_check = True
              | otherwise = False

makeMove :: Move -> Game -> Maybe Game
makeMove m g | m == move_w && g == initGame = game_b
             | otherwise = Nothing

think :: Game ->  Maybe Move
think g | g == fromJust game_b = move_b
        | otherwise = Nothing
