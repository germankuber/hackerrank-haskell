staircase number = putStrLn $  drop 1 $ foldl (\acc value -> let characters = replicate value '#'
                                                                 spaces = replicate (number - value) ' '
                                                             in acc ++ "\n" ++ spaces ++ characters) "" [1..number] 