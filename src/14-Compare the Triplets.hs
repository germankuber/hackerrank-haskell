

compareTriplets alice bob = let compareNumbers = zipWith (\a b -> if a > b
                                                                        then (1,0)
                                                                        else if b > a
                                                                            then (0,1)
                                                                            else (0,0)) alice bob
                                result = foldr (\(f,s) (k,y) -> (f+k,s+y)) (0,0) compareNumbers
                                formatResult (a,b) = [a,b]
                            in formatResult result