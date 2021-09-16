module Utils where

splitOn     :: Char -> String -> [String]
splitOn pc s | null s = []
             | length s == 1 = [s | not (pred (head s))]
             | otherwise = if null s' then [w] else w : splitOn pc (tail s')
  where (w,s') = break pred s
        pred = (pc ==)
