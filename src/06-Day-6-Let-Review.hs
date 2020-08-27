import Control.Monad



processWord word = showResult word where
    process word = foldl (\(even, odd, index) x -> if mod index 2 == 0 
                                                    then (x : even, odd, index + 1)
                                                    else (even, x : odd, index + 1)) ("","",0) word
    showResult word = let (even, odd, _) = process word
                      in (reverse even, reverse odd)



showResult :: Foldable t => t Char -> [Char]
showResult word = let (even, odd) = processWord word 
                      result = (even) ++ " " ++ (odd)
                  in  result


main :: IO()
main = solve where 

    solve = do
         number <- getLine
         result <- forM [1..read number] (\a -> do
            word <- getLine
            putStrLn $ showResult word
            return () )
         putStrLn ""
