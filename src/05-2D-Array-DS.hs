import Data.List
data GroupOfHourglass = GroupOfHourglass {
    number::Int,
    parts::[Int],
    typePart:: String
} deriving (Show, Eq)
-- hourglassSum :: [[Int]] -> Int

hourglassSum arr = let  hourglassGrouped = groupBy (\x y -> number x == number y) $ (sortBy sortGT  (processRow arr))
                        showHourglassGrouped list =  map  (map (\x-> sum $ parts x) ) list
                        showMeResult list = maximum $ map (\x -> sum $ map (\y->  sum $ parts y) x) list
                    in  showMeResult $ hourglassGrouped

sortGT a b
  | number a < number b = GT
  | number a >  number b = LT
  | number a == number b = compare (number a) (number b)


processRow :: [[Int]] -> [GroupOfHourglass]
processRow list = process list 0 where
    process (f:s:t:[]) index = let maxIndex       = minimum $ map length (f:s:[t])
                                   processCheck   = processExtremes f index maxIndex "Sup" ++ processMiddle s index maxIndex ++ processExtremes t index maxIndex "Down"
                               in processCheck
    process (f:s:t:res) index = let maxIndex      = minimum $ map length (f:s:[t])
                                    processCheck  = processExtremes f index maxIndex "Sup" ++ processMiddle s index maxIndex ++ processExtremes t index maxIndex "Down" ++ process (s:t:res) (index + maxIndex - 2)
                                in processCheck

processExtremes :: [Int] -> Int -> Int -> String -> [GroupOfHourglass]
processExtremes list index maxIndex typePart = process list index 0 maxIndex where
    process :: [Int] -> Int -> Int -> Int -> [GroupOfHourglass]
    process (f:s:t:res) index indexEach maxIndex | indexEach <= maxIndex = GroupOfHourglass index (f:s:t:[]) typePart : process (s:t:res) (index + 1) (indexEach + 1 ) maxIndex
                                                 | otherwise         = []
    process (f:s:t:[]) index indexEach maxIndex | indexEach <= maxIndex = [GroupOfHourglass index (f:s:t:[]) typePart]
                                                | otherwise         = []
    process _ index indexEach maxIndex = []


processMiddle :: [Int] -> Int -> Int -> [GroupOfHourglass]
processMiddle list index maxIndex = process list index 0 maxIndex where
    process :: [Int] -> Int -> Int -> Int -> [GroupOfHourglass]
    process (f:s:t:res) index indexEach maxIndex | indexEach <= maxIndex = GroupOfHourglass index (0:s:0:[]) "Middle" : process (s:t:res) (index + 1) (indexEach + 1) maxIndex
                                                 | otherwise         = []
    process (f:s:t:[]) index indexEach maxIndex | indexEach <= maxIndex = [GroupOfHourglass index (0:s:0:[]) "Middle"]
                                                | otherwise         = []
    process (_) _ _ _ = []
