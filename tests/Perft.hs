module Perft where

import Game
import Valid
import Play
import Data.Maybe
import Moves
import Parsing

perft :: Game -> Int -> Int
perft p d | d == 0 = 1
          | otherwise = sum $ map (`perft` (d-1)) sucPos
  where moves = genValidMoves p
        sucPos = mapMaybe (`makeMove` p) moves

main_map :: [(String, Game -> String -> IO ())]
main_map = [
   ("quit", \_ _ -> quit)
   ,("go", perfTest)
   ,("position", setPos)
  ]

quit = return ()

setPos :: Game -> String -> IO ()
setPos g str = do
           let p = parse pGame str
           if null p then mainLoop g
           else do
             let p_ = (fst . head) p
             mainLoop p_

perfTest :: Game -> String -> IO ()
perfTest p str | n == 0 = do
  putStrLn $ "0000: " ++ show 1
  putStrLn $ "Nodes: " ++ show 1
  mainLoop p
               | otherwise = showBranches moves sucPos (n-1)
  >> mainLoop p
  where n = read str
        moves = genValidMoves p
        sucPos = mapMaybe (`makeMove` p) moves

showBranches :: [Move] -> [Game] -> Int -> IO ()
showBranches ms gs n = do
  mapM_ putStrLn sss
  putStrLn $ "Total Nodes: " ++ show total
  where spb = map show npb
        npb = map (`perft` n) gs
        sms = map (++ ": ") $ map show ms
        sss = zipWith (++) sms spb
        total = sum npb

mainLoop :: Game -> IO ()
mainLoop p = do
  res <- getLine
  if null res then mainLoop p
  else do
    let cmd = words res
    let mbAction = lookup (cmd!!0) main_map
    if isNothing mbAction then mainLoop p
      else do
           let mba = fromJust mbAction
           mba p (cmd!!1)
      

main :: IO ()
main = mainLoop init_game
