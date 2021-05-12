-- Uses list-merge to efficiently intersect two lists

ages        = [1, 2, 5, 6]
names       = [1, 5, 6, 7, 8, 9]
addresses   = [2, 3, 7, 9, 10]
siblings    = [3, 5, 6, 9]

intersect [] [] = []
intersect [] _  = []
intersect _  [] = []
intersect (x:xs) (y:ys)
            | x == y    = (x:intersect xs ys)
            | x > y     = intersect (x:xs) ys
            | otherwise = intersect xs (y:ys)

main = putStrLn $ show $ intersect addresses siblings