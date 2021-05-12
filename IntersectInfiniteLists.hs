-- Intersect an arbitrary number of ordered lists
-- based on a problem by @alecthomas


intersectTwo [] []         = []
intersectTwo ls []         = []
intersectTwo [] ls         = []
intersectTwo (x:xs) (y:ys)
             | x == y      = [x] ++ (intersectTwo xs ys)
             | x > y       = intersectTwo (x:xs) ys
             | x < y       = intersectTwo xs (y:ys)

intersect []               = []
intersect ls               = foldr intersectTwo (ls !! 0) ls


-- Intersect as many lists as you like here!
main = putStrLn $ show $ intersect [[1, 2, 3, 4], [2, 3], [2, 3, 4], [2, 3, 4, 6]]


