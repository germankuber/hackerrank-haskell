
data BombPosition = BombPosition{
    timePut :: Int,
    hasBomb :: Bool
} deriving (Show)  

data Position = Position {
    x :: Int,
    y :: Int,
    bomb :: Bool
} deriving (Show)  

bomberMan seconds grid = processGame grid 1
    


processGame :: [String] -> Int -> [Position]
processGame [] _= []
processGame (l:res) y = getBombPositions l 1 y ++ processGame res (y + 1)


getBombPositions :: String -> Int -> Int -> [Position]
getBombPositions [] _ _= []
getBombPositions (c:res) x y | c == 'O'  = Position x y True : getBombPositions res (x+1) y
                             | otherwise = getBombPositions res (x+1) y



-- processGame :: Int -> [[BombPosition]] -> [[BombPosition]]
-- processGame 0 grid =
-- processGame actualSecond finalSecond grid | finalSecond - actualSecond = 
-- processGame (seconds - 1) grid


-- processGrid

-- becomeList :: [String] -> [[BombPosition]]
-- becomeList = map processLine


-- processLine :: String -> [BombPosition]
-- processLine [] = []
-- processLine (c:res) | c == '0'  = BombPosition 0 False : processLine res
--                     | otherwise = BombPosition 0 True : processLine res


