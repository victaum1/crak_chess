module PvTable where

import Data.Maybe (isNothing, maybeToList, fromJust)
import Data.Word ( Word64 )
import Data.Array.IArray ( Array, (//), array, IArray(bounds), (!) )
import MyHash (zHash)
-- import Squares ()
import Moves ( Move, null_move, std_w_move, std_pv )
import Game ( Game, init_game)
import Play (makeMove)

-- simple types

type PosKey = Int

type PvTable = Array PosKey Move


-- funcs

initPvTable :: Int -> PvTable
initPvTable n = array (0,entries) [(i,null_move) | i<-[0..entries]]
  where entries = 2^round(logBase 2 (fromIntegral n)) - 1


getPvEntries :: PvTable -> Int
getPvEntries ip = snd (bounds ip) + 1

getPvIndex :: Game -> PvTable -> Int
getPvIndex ig ip = fromIntegral gh  `mod` entries
  where gh = zHash ig
        entries = getPvEntries ip

storePvMove :: Game -> Move -> PvTable -> PvTable
storePvMove ig im ip | isNothing og = ip
                     | otherwise = op

  where op = ip // [(index,im)]
        og = makeMove im ig
        index = getPvIndex ig ip


storeGameMove :: (Game,Move) -> PvTable -> PvTable
storeGameMove = uncurry storePvMove


makePvGames :: Game -> [Move] -> [Game]
makePvGames ig [] = [ig] -- only to protect the type
makePvGames ig (im:ms) | isNothing ng = []
                       | otherwise = ig : makePvGames (fromJust ng) ms
  where ng = makeMove im ig

storePvLine :: Game -> [Move] -> PvTable -> PvTable
storePvLine ig [] ip = ip
storePvLine ig (im:ms) ip | isNothing ng = ip
                          | otherwise = storePvLine jg ms np
  where np = storePvMove ig im ip
        ng = makeMove im ig
        jg = fromJust ng


probePvMove :: Game -> PvTable -> Maybe Move
probePvMove ig ip | null_move == om = Nothing
                  | otherwise = Just om
  where om = ip ! index
        entries = getPvEntries ip
        index = getPvIndex ig ip


getPvLine :: Game -> PvTable -> [Move]
getPvLine = iterPVLine []

iterPVLine :: [Move] -> Game -> PvTable -> [Move]
iterPVLine ms ig ip | null sin || ng == [Nothing] = ms
                    | null ms = if null sin || ng == [Nothing] then ms else
                        iterPVLine sin (fromJust (head ng)) ip
                    | otherwise = iterPVLine (ms ++ sin)
                      (fromJust (head ng)) ip
  where sin = maybeToList probed
        probed = probePvMove ig ip
        ng = map (`makeMove` ig) sin


showPVLine :: [Move] -> String
showPVLine = concatMap ((++ " ") . show)
