-- Finds the intersection of two rectangles, returned as a rectangle.
--

data Rect = Rect { x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
} | NoRect deriving Show

-- Returns the intersection rectangle or NoRect if they don't intersect
intersect first second
    | width intersection <= 0   = NoRect
    | height intersection <= 0  = NoRect
    | otherwise                 = intersection
    where
        intersection = intersectRect first second


intersectRect first second = Rect {
    x = max r1x1 r2x1,
    y = max r1y1 r2y1,
    width = (min r1x2 r2x2) - (max r2x1 r1x1),
    height = (min r1y2 r2y2) - (max r2y1 r1y1)
}
    where
        -- Enumerate all the vertices for clarity
        r1x1 = x first
        r1x2 = x first + width first
        r1y1 = y first
        r1y2 = y first + height first
        r2x1 = x second
        r2x2 = x second + width second
        r2y1 = y second
        r2y2 = y second + height second

main = putStrLn $ show $ intersect (Rect 5 5 2 2) (Rect 2 2 4 4)