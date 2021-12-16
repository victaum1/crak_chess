module Evaluate where

import Game ( Game, turn, board )
import Board ( Board, whereIsPiece )
import Pieces ( all_white_pieces, all_black_pieces )

-- types
type Score = Int

-- vars
inf_score = 4000 :: Score
material_w = 3900:: Score


--funcs
nWhitePieces :: Board -> Int
nWhitePieces b = sum (map (length . (`whereIsPiece` b))
                      all_white_pieces)


nBlackPieces :: Board -> Int
nBlackPieces b = sum (map (length . (`whereIsPiece` b))
                      all_black_pieces)

who2Move g | si = 1
           | otherwise = -1
  where si = turn g

evaluate :: Game -> Score
evaluate g = evalMaterial g*1 + evalSpace g*0

evalMaterial :: Game -> Score
evalMaterial g = material_w *
  (nWhitePieces b - nBlackPieces b) * who2Move g
  where b = board g

evalSpace :: Game -> Score
evalSpace g = 0
