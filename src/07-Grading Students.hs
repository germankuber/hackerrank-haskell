
nextMultipleOf :: Int -> Int -> Int
nextMultipleOf multiple startNumber | mod startNumber multiple == 0 = startNumber
                                    | otherwise = nextMultipleOf multiple (startNumber + 1)


gradingStudents :: [Int] -> [Int]
gradingStudents scores = showSolution scores where

                        showSolution s = map mapScores s
                        mapScores :: Int -> Int
                        mapScores score | score < 38 = score
                                        | otherwise = let nextMultiple = nextMultipleOf 5 score
                                                          applyPlusOrNot difference | difference < 3 =  nextMultiple
                                                                                    | otherwise = score
                                                      in applyPlusOrNot  $ (nextMultiple - score)