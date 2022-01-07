module Evaluate where

import Game ( Game, turn, board )
import Board ( Board, whereIsPiece, checkPiece_, whereIsKing )
import Pieces ( all_white_pieces, all_black_pieces, piece_types, PieceType (Pawn), Piece (MkPiece) )
import Generator ( genPawnCaptureSquares, genKingSimpleSquares, moveGenBySide_ )
import Valid ( isMate, isDraw )

-- types
type Score = Int

-- vars
max_material = 3900:: Score
mate_score = max_material + 100
inf_score = max_material + 1
_inf_score = negate inf_score
material_w = max_material

score_pieces = [100,300,300,500,900,0]


material_map = zip piece_types score_pieces

--funcs
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

ratMaterial :: Game -> Rational
ratMaterial g = fromIntegral (evalMaterial g) / fromIntegral max_material


evalMaterial :: Game -> Score
evalMaterial g | si = materialWhite - materialBlack
           | otherwise = materialBlack - materialWhite
  where si = turn g
        bd = board g
        materialWhite = sum $ zipWith (*) score_pieces
          (map (length . (`checkPiece_` bd)) all_white_pieces)
        materialBlack = sum $ zipWith (*) score_pieces
          (map (length . (`checkPiece_` bd)) all_black_pieces)


spaceEval :: Game -> Rational
spaceEval g | si = fromIntegral (spaceWhite g - spaceBlack g) / 71
        | otherwise = fromIntegral (spaceBlack g - spaceWhite g) / 71
  where si = turn g


spaceWhite g = length (moveGenBySide_ True bd) + length
  (genKingSimpleSquares wsk bd) + length (concatMap (flip (genPawnCaptureSquares True) bd) wap)
  where bd = board g
        wsk = whereIsKing True bd
        wap = whereIsPiece (MkPiece (True,Pawn)) bd

spaceBlack g = length (moveGenBySide_ False bd) + length
  (genKingSimpleSquares wsk bd) + length (concatMap (flip (genPawnCaptureSquares False) bd) wap)
  where bd = board g
        wsk = whereIsKing False bd
        wap = whereIsPiece (MkPiece (False,Pawn)) bd


eval_factors :: [Rational]
eval_factors = [3/4,1/4]
eval_funcs = [ratMaterial, spaceEval]

evaluate :: Game -> Score
evaluate g | isMate g = negate mate_score
           | isDraw g = 0
           | otherwise = heuristic g
  where si = turn g


heuristic = matAndSp

matAndSp :: Game -> Score
matAndSp g = round $ fromIntegral max_material * sum (zipWith (*) eval_factors (map ($ g) eval_funcs))

