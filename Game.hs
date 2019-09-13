module Game (Game(..), fen2Game, game2FEN, initGame) where

import Data.Maybe
import Squares
import Board
import Parsing

initFEN = initBoardFEN ++ " w KQkq - 0 1"

data Turn = First | Second
  deriving (Eq,Show)

data Game = Game {
    getBoard  :: Board
  , getTurn   :: Turn
  , getCastle :: Int
  , getEpSq   :: Maybe Square
  , getNPlys   :: Int
  , getNMoves  :: Int
  } deriving Show

pTurn :: Parser Turn
pTurn = P(\inp -> case inp of
                      [] -> []
                      (c:cs) -> if c == 'w' then [(First,cs)]
                                else if c == 'b' then [(Second,cs)]
                                else []) 

castleChars = "KQkq"
castleCodes = [1,2,4,8]

castleTable = zip castleChars castleCodes
castleTable' = zip castleCodes castleChars

packCastle :: String -> Int -> String -> Maybe Int
packCastle [] n _ = Just n
packCastle (c:cs) n xs | isInTable = packCastle cs nCastle (xs++[c])
                       | otherwise = Nothing
                  where findCastle = lookup c castleTable
                        already = elem c xs
                        isInTable = (isJust findCastle) && (not already)
                        nCastle = n + fromJust (findCastle)

pCastle :: Parser Int
pCastle = do
            x <- (many letter) <|> (string "-")
            if x == "-" then return 0
            else return (fromJust $ packCastle x 0 "")

pPlys :: Parser Int
pPlys = do
          n <- natural
          return n

pMoves :: Parser Int
pMoves = do
           n <- natural
           return n

pGame :: Parser Game
pGame = do
  bd <- pBoard
  space
  t <- pTurn
  space
  c <- pCastle
  space
  sq <- pSquare
  space
  ps <- pPlys
  space
  ms <- pMoves
  return (Game bd t c sq ps ms)

fen2Game :: String -> Game
fen2Game str = fst $ head $ parse pGame str

initGame = fen2Game initFEN

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
               where isInTable = isJust $ lookup n castleTable'
                     findChar = fromJust $ lookup n castleTable'

game2FEN :: Game -> String
game2FEN g = unwords [board2FEN $ getBoard $ g,turn,cf,sq,plys,moves] 
            where turn = if (getTurn g) == First then "w" else "b"
                  plys = show $ getNPlys g
                  cf = showCflags $ getCastle g
                  sq = if isJust (getEpSq g) then show (fromJust(getEpSq g))
                       else  "-"
                  moves = show $ getNMoves g

