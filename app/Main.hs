module Main where

import Lib

main :: IO ()
main = putStrLn $ kangaroo 0 3 4 2



kangaroo x1 v1 x2 v2 = solve x1 v1 x1 0 x2 v2 x2 0 0 where

            solve x1 v1 m1 j1 x2 v2 m2 j2 line | x1 > x2 && v1 > v2 = "NO"
                                               | x2 > x1 && v2 > v1 = "NO"
                                               | m1 == m2  = "YES"
                                               | m1 /= m2 && j1 == j2 && j1 /= 0 && j2 /= 0 = "NO"
                                               | line >= m1 && line >= m2 = solve x1 v1 (m1 + v1) (j1 + 1) x2 v2 (m2 + v2) (j2 + 1)  (line + 1) 
                                               | line >= m1 = solve x1 v1 (m1 + v1) (j1 + 1)  x2 v2 m2 j2 (line + 1) 
                                               | line >= m2 = solve x1 v1 m1 j1 x2 v2 (m2 + v2) (j2 + 1) (line + 1) 
                                               | otherwise  = solve x1 v1 m1 j1 x2 v2 m2 j2 (line + 1) 


    
