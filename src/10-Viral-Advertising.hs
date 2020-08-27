
viralAdvertising n  = sum $ solve n where
                      solve number = foldl (\(f:res) x -> div (f * 3) 2 : (f:res)) [2] [2..number]