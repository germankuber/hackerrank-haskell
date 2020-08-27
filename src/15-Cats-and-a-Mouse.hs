catAndMouse a b c | abs (c - a ) >  abs (c - b) = "Cat B"
                  | abs (c - a) <  abs (c - b) = "Cat A"
                  | otherwise = "Mouse C"