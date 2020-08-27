module ElectronicsShop where
import Data.List

getMoneySpent :: (Num t, Ord t) => [t] -> [t] -> t -> t
getMoneySpent keyboards drivers money = recursionSolution (sort keyboards) (sort drivers) money (-1) 


solveProblem :: (Ord a, Num a) => a -> [a] -> a -> a -> a
solveProblem k [d] money biggerSolution | k + d < money && k + d > biggerSolution = k + d
                                        | otherwise = biggerSolution  
solveProblem k (d:drivers) money biggerSolution | k + d < money && k + d > biggerSolution   = solveProblem k drivers money (k + d) 
                                                | k + d < money && k + d <= biggerSolution  = solveProblem k drivers money biggerSolution 
                                                | k + d > money                             = biggerSolution 
                                                | k + d == money                            = money 

recursionSolution [] _ _ biggerSolution = biggerSolution
recursionSolution (k:keyboards) (d:drivers) money biggerSolution    | k + d >= money = biggerSolution 
                                                                    | otherwise = let possibleBiggerSolution = solveProblem k (d:drivers) money (k + d)
                                                                                      nextBigSolution   | possibleBiggerSolution > biggerSolution   = possibleBiggerSolution
                                                                                                        | otherwise                                 =  biggerSolution
                                                                                    in recursionSolution keyboards (d:drivers) money nextBigSolution 


    