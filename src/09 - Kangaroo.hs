module Kangaroo
    (kangaroo ) where
import Debug.Trace

-- kangaroo :: Int -> Int -> Int -> Int -> String
kangaroo x1 v1 x2 v2 = solve x1 v1 x1 0 x2 v2 x2 0 0 where
            solve x1 v1 m1 j1 x2 v2 m2 j2 line | x1 > x2 && v1 > v2 = "NO"
                                               | x2 > x1 && v2 > v1 = "NO"
                                               | m1 == m2  = "YES"
                                               | m1 /= m2 && j2 > j1  = "NO"
                                               | line >= m1 && line >= m2 = solve x1 v1 (m1 + v1) (j1 + 1) x2 v2 (m2 + v2) (j2 + 1)  (line + 1) 
                                               | line >= m1 = solve x1 v1 (m1 + v1) (j1 + 1)  x2 v2 m2 j2 (line + 1) 
                                               | line >= m2 = solve x1 v1 m1 j1 x2 v2 (m2 + v2) (j2 + 1) (line + 1) 
                                               | otherwise  = solve x1 v1 m1 j1 x2 v2 m2 j2 (line + 1) 

-- 0 3 4 2
-- x1 = 9
-- v1 = 3
-- j1 = 3
-- x2 = 8
-- v2 = 2
-- j2 = 2

-- 0 1 2 3 4 5 6 7
-- 0 3 4 2



-- solve x1 v1 m1 j1 x2 v2 m2 j2  line | m1 == m2 && j2 == j1 && j1 > 0 && j2 > 0 = "YES"
-- | m1 /= m2 && j2 == j1 && j1 > 0 && j2 > 0 = trace("1") "NO" 
-- | m1 /= m2 && j2 > j1 && j1 > 0 && j2 > 0 = trace("1") "NO" 
-- | x1 > x2 && v1 > v2 = trace("2") "NO" 
-- | x2 > x1 && v2 > v1 = trace("3") "NO" 
-- | line >= x1 && line >= x2 = solve x1 (m1 + v1) v1 (j1 + 1) x1 v2 (m2 + v2) (j2 + 1) (line + 1)
-- | line >= x1 = solve x1 (m1 + v1) v1 (j1 + 1) x1 v2 m2 j2 (line + 1)
-- | line >= x2 = solve x1 m1 v1 j1 x1 v2 (m2 + v2) (j2 + 1) (line + 1)
-- | otherwise = solve x1 v1 m1 j1 x2 v2 m2 j2 (line +1)