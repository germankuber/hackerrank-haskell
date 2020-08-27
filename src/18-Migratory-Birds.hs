import System.IO
import Data.List
import Data.Function

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    types_temp <- getLine
    let types = map read $ words types_temp :: [Int]
    print $ head . head . reverse . sortBy (compare `on` length) . group . sortBy (flip compare) $ types