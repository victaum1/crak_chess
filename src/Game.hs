module Game where

import           Board
import           Data.Maybe
import           Data.Either
import           Parsing
import           Pieces
import           Squares
import           Utils

-- vars / const
init_fen = init_board_fen ++ "w KQkq - 0 1"
castle_chars = "KQkq"
castle_codes = [1,2,4,8] :: [Int]
init_game = myRight $ fen2Game init_fen
castle_table = zip castle_chars castle_codes
castle_table' = zip castle_codes castle_chars

-- adts
type CastleFlag = Int
type Nplys = Int
type Nmoves = Int
type GameState = (Board,Side,CastleFlag,Maybe Square,Nplys,Nmoves)

-- data GameState = MkGameState {
--      board :: Board
--     ,turn :: Side
--     ,castleFlag :: castleFlag
--     ,epSquare :: Maybe Square
--     ,nPlys :: Int
--     ,nMoves :: Int
--   } deriving (Eq)

newtype Game = MkGame {fromGame::GameState} deriving (Eq)

board (MkGame (b,_,_,_,_,_)) = b
turn (MkGame (_,t,_,_,_,_)) = t
castleFlag (MkGame (_,_,c,_,_,_)) = c
epSquare (MkGame (_,_,_,e,_,_)) = e
nPlys (MkGame (_,_,_,_,p,_)) = p
nMoves (MkGame (_,_,_,_,_,m)) = m


showGame :: Game -> String
showGame (MkGame (bd,si,cf,ep,ps,ms)) = "Game{ " ++ show bd ++ "," ++ show si ++ "," ++ show cf ++ "," ++ show ep ++ "," ++ show ps ++ "," ++ show ms ++ "}" 

instance Show Game where
  show a = showGame a

-- funcs 
packCastle :: String -> Int -> String -> Maybe Int
packCastle [] n _ = Just n
packCastle (c:cs) n xs | isInTable = packCastle cs nCastle (xs++[c])
                       | otherwise = Nothing
                  where findCastle = lookup c castle_table
                        already = c `elem` xs
                        isInTable = isJust findCastle && not already
                        nCastle = n + fromJust findCastle

fen2Game :: String -> Either ParseError Game
fen2Game = parse pGame ""

showCflags :: Int -> String
showCflags n | isInTable = [findChar]
             | n == 3 = "KQ"
             | n == 5 = "Kk"
             | n == 6 = "Qk"
             | n == 7 = "KQk"
             | n == 9 = "Kq"
             | n == 10 = "Qq"
             | n == 12 = "kq"
             | n == 13 = "Kkq"
             | n == 15 = "KQkq"
             | otherwise = "-"
               where isInTable = isJust $ lookup n castle_table'
                     findChar = fromJust $ lookup n castle_table'

game2FEN :: Game -> String
game2FEN g = unwords [board2FEN getBoard,getTurn,getCf,getSq,getPlys,getMoves]
            where getBoard = board g
                  getTurn = if turn g then "w" else "b"
                  getPlys = show $ nPlys g
                  getCf = showCflags $ castleFlag g
                  getSq = maybe "-" show $ epSquare g
                  getMoves = show $ nMoves g
-- parsing
pPlys :: GenParser Char st Int
pPlys = pNat

pNmoves :: GenParser Char st Int
pNmoves = pNat

pNoSq :: GenParser Char st Char
pNoSq = char '-'

pEpSq :: GenParser Char st (Maybe Square)
pEpSq = do
          try pNoSq >> return Nothing
        <|>
        do
          Just <$> pSquare

pTurn :: GenParser Char st Side
pTurn = do
  try (sat (=='w')) >> return True
  <|>
  do
  sat (=='b')
  return False

pCastle :: GenParser Char st Int
pCastle = do
            try (char '-') >> return 0
          <|>
          do
            x <- many (oneOf castle_chars)
            return (fromJust $ packCastle x 0 "")

pGame :: GenParser Char st Game
pGame = do
  bd <- pFenBoard
  t <- pToken pTurn
  c <- pCastle
  sq <- pToken pEpSq
  ps <- pPlys
  space
  nm <- pNmoves
  return (MkGame (bd,t,c,sq,ps,nm))
