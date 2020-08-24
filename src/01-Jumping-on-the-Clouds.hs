jumpingOnClouds clouds = filterClouds clouds

type Cumulus = Int 
type Thunderheads = Int 
filterClouds clouds = let listOfAllClouds = zip clouds [0..(length clouds)]
                          (cumulus,thunderheads) = foldr (\ (a,b) (cumulusIn,thunderheadsIn) -> if a == 0
                                                                      then (b : cumulusIn,thunderheadsIn)
                                                                      else (cumulusIn,b:thunderheadsIn) ) ([],[]) listOfAllClouds
                    --   in   cumulus
                      in   (length $ processClouds 0  cumulus) - 1

processClouds :: Int -> [Int]  -> [Int]
processClouds prev (f:s:t:res) | s - f == 2 = f : (processClouds f (s:t:res) )
                               | s - f == 1 && t - f == 2 = f : t : (processClouds t (res) )
                               | s - f == 1 && t - f > 2 = f : s : (processClouds s (t: res) )
                               | otherwise = []
processClouds prev (f:s:t:[]) | s - f == 2 = f : (processClouds f (s:[t]) )
                              | s - f == 1 && t - f == 2 = f : t : []
                              | s - f == 1 && t - f > 2  && t - s > 2 = f : s : []
                              | s - f == 1 && t - f > 2  && t - s <= 2 = f : s : [t]
                              | otherwise = []

processClouds prev (f:s:[]) | s - f <= 2 =  f : [s]
                        | otherwise = [f]
processClouds prev (f:[]) | f - prev <= 2 = [f]
                          | otherwise = []
processClouds prev [] = []