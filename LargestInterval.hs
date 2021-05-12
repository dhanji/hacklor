-- Largest interval algorithm.
--     "Compute the largest profit to be gained from a series of prices"
--

profits smallest largest gains []     = gains
profits smallest largest gains (p:ps)
    | p < smallest  = profits p p (gains ++ [largest - smallest]) ps
    | p > largest   = profits smallest p (gains ++ [p - smallest]) ps
    | otherwise     = profits smallest largest gains ps


bestProfit []       = 0
bestProfit ls       = foldr max (0) (profits first first [] ls)
    where
        first       = ls !! 0


main = putStrLn $ show $ bestProfit prices
    where
        prices      = [10, 7, 5, 8, 11, 9, 4, 20]