-- Floyd's algorithm to detect a cycle in a linked list
--    Also called the "Tortoise & Hare" algorithm
--    This increments two pointers at different rates, if
--    the second laps the first, then we have a cycle.

cyclical pick                             = iterateList 0 1 pick
    where
        iterateList tortoise hare pick
            | pick tortoise == -1         = False
            | pick hare == -1             = False
            | pick tortoise == pick hare  = True
            | otherwise                   = iterateList (tortoise + 1) (hare + 2) pick


-- Methodology: the chosen picker function makes the list cyclical or not
main = putStrLn $ show $ cyclical (normalPicker list)
    where
        list                  = [1, 2, 3, 5, 6, 7]
        normalPicker ls i
            | i < length ls   = ls !! i
            | otherwise       = -1
        cyclicPicker ls i
            | i < length ls   = ls !! i
            | otherwise       = ls !! (i `mod` (length ls))