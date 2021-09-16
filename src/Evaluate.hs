module Evaluate where

-- import Game
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Board
-- import Pieces
-- import Valid
-- import Generator

import Game ( Game, GameState(board, turn) )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Board ( checkPiece_, whereIsPiece, whereIsKing )
import Pieces (Piece(Piece), all_black_pieces, all_white_pieces, piece_types, PieceType (Pawn,Knight) )
import Valid ( isDraw, isMate )
import Generator (genKingMoves, moveGenBySide_, genKingSimpleSquares,
                  genPawnCaptureSquares)
import Moves

type Delta = Int
type Score = Int

max_material_score = 3900

score_pieces = [100,300,300,500,900,0]

mate_score :: Score
mate_score = max_material_score+100

material_map = Map.fromList
  $ zip piece_types score_pieces


simpleMaterial :: Game -> Rational
simpleMaterial g | si = fromIntegral
  (countAllWhites g - countAllBlacks g)/15
                 | otherwise = fromIntegral
  (countAllBlacks g - countAllWhites g)/15
  where si = turn g

countAllWhites g = sum  $ map length sqs
  where bd = board g
        sqs = map (`whereIsPiece` bd) all_white_pieces

countAllBlacks g = sum  $ map length sqs
  where bd = board g
        sqs = map (`whereIsPiece` bd) all_black_pieces


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
space g | si = spaceWhite g - spaceBlack g
        | otherwise = spaceBlack g - spaceWhite g
  where si = turn g

spaceWhite g = length (moveGenBySide_ True bd) + length
  (genKingSimpleSquares wsk bd) + length (concatMap (flip (genPawnCaptureSquares True) bd) wap)
  where bd = board g
        wsk = whereIsKing True bd
        wap = whereIsPiece (Piece True Pawn) bd

spaceBlack g = length (moveGenBySide_ False bd) + length
  (genKingSimpleSquares wsk bd) + length (concatMap (flip (genPawnCaptureSquares False) bd) wap)
  where bd = board g
        wsk = whereIsKing False bd
        wap = whereIsPiece (Piece False Pawn) bd

evalPos :: Game -> Score
evalPos g | isMate g = negate mate_score
          | isDraw g = 0
          | otherwise = matAndSp g
  where si       = turn g


matAndSp :: Game -> Score
matAndSp g = round $ fromIntegral max_material_score / 2
  * (simpleMaterial g
  + fromIntegral (space g) / 71)
