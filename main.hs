-- Median of two sorted lists

med [] = 0
med ls
  | length ls `mod` 2 == 0    = fromIntegral ((ls !! mid) + (ls !! mid - 1)) / 2
  | otherwise                 = fromIntegral $ ls !! mid
  where
      mid = (length ls) `div` 2


merge [] ls = ls
merge ls [] = ls
merge (x:xs) (y:ys)
  | x == y    = x: y: merge xs ys
  | x < y     = x: merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys


median l1 l2 = med $ merge l1 l2

main = putStrLn $ show $ median [0, 0] [0, 0]
