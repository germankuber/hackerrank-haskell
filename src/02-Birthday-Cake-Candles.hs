import Data.List
birthdayCakeCandles :: [Int] -> Int
birthdayCakeCandles c = let maxValue = maximum c
                            resultV = length $ filter (\x -> x == maxValue) c
                        in resultV