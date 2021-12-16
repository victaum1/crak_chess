module ManualPerft where

import Data.Either
import Data.Maybe
import Game
import Valid
import Play
import Moves
import Parsing
import Perft
import Utils

main_map :: [(String, Game -> String -> IO ())]
main_map = [
   ("quit", \_ _ -> quit)
   ,("go", perfTest)
   ,("position", setPos)
  ]

quit = return ()

setPos :: Game -> String -> IO ()
setPos g str = do
           let p = parse pGame str ""
           if isRight p then mainLoop g
           else do
             let p_ = myRight p
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
  putStrLn $ "Total: " ++ show total
  where spb = map show npb
        npb = map (`perft` n) gs
        sms = map ((++ ": "). show) ms
        sss = zipWith (++) sms spb
        total = sum npb

mainLoop :: Game -> IO ()
mainLoop p = do
  res <- getLine
  if null res then mainLoop p
  else do
    let cmd = words res
    let mbAction = lookup (head cmd) main_map
    if isNothing mbAction then mainLoop p
      else do
           let mba = fromJust mbAction
           mba p (unwords $ drop 1 cmd)

main :: IO ()
main = do
  putStrLn $ "Peft Testing..."
  mainLoop init_game
