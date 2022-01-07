module MyHash where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word ( Word64 )
import Data.Bits(xor)
import System.Random ( mkStdGen, Random(randoms), StdGen )
import Pieces (all_pieces, Piece)
import Squares (Square(MkSquare), squareFile)
import Board (Board, whereIsPiece)
import Game (Game, castleFlag, board, turn, epSquare)

type Zkey = Word64

-- vars
-- zobrist
z_seed = 555::Int

z_nums :: [Zkey]
z_nums = srNumbers 600 (mkStdGen z_seed)

z_squares = map MkSquare $ (,) <$> [0..7] <*> [1..6]

z_piece_sqs = (,) <$> all_pieces <*> z_squares

z_pos_map = zip z_piece_sqs $ take 576 z_nums

z_side = (False, z_nums !! 576)

z_castle = zip [1..15] (take 15 $ drop 577 z_nums)

z_ep = zip [0..7] (take 8 $ drop 592 z_nums)


-- funcs
srNumbers :: Int -> StdGen -> [Zkey]
srNumbers n g = take n $ randoms (mkStdGen z_seed)

zHash :: Game -> Zkey
zHash g = foldl xor 0 (game2Keys g)

game2Keys :: Game -> [Zkey]
game2Keys g = board2Keys b ++ side2Keys s ++ castle2Keys cf
              ++ ep2Keys ep
  where b = board g
        cf = castleFlag g
        s = turn g
        ep = maybe (-1) squareFile (epSquare g)


side2Keys :: Bool -> [Zkey]
side2Keys s | s = []
            | otherwise = [snd z_side]

castle2Keys :: Int -> [Zkey]
castle2Keys n | n `elem` [1..15] = [fromMaybe 0 (lookup n z_castle)]
              | otherwise = []

ep2Keys :: Int -> [Zkey]
ep2Keys ep | ep `elem` [0..7] = [fromMaybe 0 (lookup ep z_ep)]
           | otherwise = []

board2Keys :: Board -> [Zkey]
board2Keys b = mapMaybe (`lookup` z_pos_map) psqs
 where sqs = map (`whereIsPiece` b) all_pieces
       psq = zip all_pieces sqs
       psqs = concatMap genPieceSquares psq

genPieceSquares :: (Piece,[Square]) -> [(Piece,Square)]
genPieceSquares (p,sqs) = (,) <$> [p] <*> sqs
