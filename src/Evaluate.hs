module Evaluate where

import Game
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Board
import Pieces
import Valid
import Generator


type Score = Int

max_material_score = 3900

score_pieces = [100,300,300,500,900,0]

material_map = Map.fromList
  $ zip piece_types score_pieces


material :: Game -> Score
material g | si = materialWhite - materialBlack
           | otherwise = materialBlack - materialWhite
  where si = turn g
        bd = board g
        materialWhite = sum $ zipWith (*) score_pieces
          (map (length . (`checkPiece_` bd)) all_white_pieces)
        materialBlack = sum $ zipWith (*) score_pieces
          (map (length . (`checkPiece_` bd)) all_black_pieces)


space :: Game -> Score
space g = 0


isMate :: Game -> Bool
isMate g | isInCheck g && null (genValidMoves g) = True
         | otherwise = False


isStaleMate g | not (isInCheck g) && null (genValidMoves g) = True
              | otherwise = False


evalPos g | isMate g = negate (max_material_score + 1)
          | isStaleMate g = 0
          | otherwise = matAndSp
  where si       = turn g
        matAndSp = material g + space g
