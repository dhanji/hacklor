-- Median of two sorted lists

median [] = 0
median ls
  | length ls `mod` 2 == 0    = fromIntegral ((ls !! mid) + (ls !! mid - 1)) / 2
  | otherwise                 = fromIntegral $ ls !! mid
  where
      mid = (length ls) `div` 2




main = putStrLn $ show $ median [1, 2, 3, 4]