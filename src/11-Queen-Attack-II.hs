-- queensAttack :: Int -> Int -> Int -> Int -> [[Int]] -> Int

data LineGame = LineGame {
    number:: Int,
    line :: [(Int, Int)]
} deriving (Show, Eq)


queensAttack boardSize numberOfObstacles columnQueenPosition rowQueenPosition  obstacles = processGame boardSize rowQueenPosition  columnQueenPosition (map (\(x:y:_) -> (y,x)) obstacles)

processGame size x y obstacles = let winLines = generateLines size x y
                                     processLine ::  [(Int, Int)] -> [(Int, Int)]
                                     processLine [] = []
                                     processLine ((x,y):res) = if  (x,y) `elem` obstacles
                                                            then []
                                                            else (x,y) : processLine res

                                     eachLines :: [LineGame] -> [(Int,Int)]
                                     eachLines [] = []
                                     eachLines lines = foldl (\acc x ->   processLine (line x) ++ acc) [] lines
                                -- in generateLines size x y
                                in (length $ filter (\(xa,ya) -> (xa,ya) /= (x,y))  $ eachLines $ generateLines size x y)
                                -- in filter (\(xa,ya) -> (xa,ya) /= (x,y))  $ eachLines $ generateLines size x y
                                -- in generateLines size x y

subtractOne :: Int -> Int
subtractOne x = x - 1
plusOne :: Int -> Int
plusOne x = x + 1

getDistanceToMove boardSize queenPositionX queenPositionY | boardSize - queenPositionX  < boardSize - queenPositionY = boardSize - queenPositionX 
                                                          | otherwise =  boardSize - queenPositionY

generateLines :: Int -> Int -> Int -> [LineGame]
generateLines boardSize queenPositionX queenPositionY = let line1 =generateLineOfAttack queenPositionX queenPositionY 1 (boardSize -  queenPositionY )   id (+1) 1 --- Arriba
                                                            line2 = generateLineOfAttack queenPositionX queenPositionY 1 (boardSize -  queenPositionX ) (+1) id 2 -- Derecha
                                                            line3 = generateLineOfAttack queenPositionX queenPositionY 1 (queenPositionY - 1) id subtractOne 3 -- Abajo
                                                            line4 = generateLineOfAttack queenPositionX queenPositionY 1 (queenPositionX - 1) subtractOne id 4 -- Izquierda
                                                            line5 = generateLineOfAttack queenPositionX queenPositionY 1 moveUpRight (+1) (+1) 5 -- diagonal derecha arriba
                                                            line6 = generateLineOfAttack queenPositionX queenPositionY 1 moveDownRight (+1) subtractOne 6 -- diagonal derecha abajo
                                                            line7 = generateLineOfAttack queenPositionX queenPositionY 1 moveDownLeft subtractOne subtractOne 7 -- diagonal izquierda abajo
                                                            moveUpRight = if boardSize - queenPositionX  < boardSize -queenPositionY
                                                                then boardSize - queenPositionX 
                                                                else boardSize -queenPositionY
                                                            moveDownRight = if boardSize - queenPositionX  < queenPositionY
                                                                then boardSize - queenPositionX 
                                                                else queenPositionY -1
                                                            moveDownLeft = if queenPositionX > queenPositionY
                                                                then queenPositionY - 1
                                                                else queenPositionX - 1
                                                            moveUpLeft = if queenPositionX > (boardSize - queenPositionY)
                                                                         then boardSize - queenPositionY
                                                                         else queenPositionX -1
                                                            line8 = generateLineOfAttack queenPositionX queenPositionY 1  (moveUpLeft ) subtractOne plusOne 8 -- diagonal izquierda arriba
                                                        in [line1, line2, line3, line4, line5, line6, line7, line8]

generateLineOfAttack ::  Int -> Int -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int -> LineGame
generateLineOfAttack queenPositionX queenPositionY from to operatorX operatorY lineIndex = let foldList = (\((row,col):res) _ -> (operatorX row, operatorY col) : ((row,col):res))
                                                                                                  in LineGame lineIndex (reverse $ foldl  foldList [(queenPositionX,queenPositionY)] [from..to])