import Data.List

palindromeIndex :: Eq a => [a] -> Int
palindromeIndex list = checkSameValue list list 0 (reverse list) ((length list) - 1)


theListIsPalindrome [] _ = True
theListIsPalindrome _ [] = True
theListIsPalindrome (a:xs) (b:ys) | a /= b    = False
                                  | otherwise = theListIsPalindrome xs ys


checkSameValue original (a:resList) index (b:resReverseList) indexReverse | a /= b = let newArrayWithoutHead = takeOutElement index original
                                                                                         newArrayWithoutLst = takeOutElement indexReverse original
                                                                                         isWithoutHeadPalindrome = theListIsPalindrome newArrayWithoutHead (reverse newArrayWithoutHead)
                                                                                         isWithoutLastPalindrome = theListIsPalindrome newArrayWithoutLst (reverse newArrayWithoutLst)
                                                                                     in if (isWithoutHeadPalindrome)
                                                                                         then index
                                                                                         else if isWithoutLastPalindrome
                                                                                            then indexReverse
                                                                                        else -1
                                                                          | otherwise = checkSameValue original resList (index + 1) resReverseList (indexReverse - 1)
checkSameValue _ [] _ _ _ = -1
checkSameValue _ _ _ [] _ = -1




takeOutElement :: Int -> [a] -> [a]
takeOutElement i array = removeElem $ splitAt i array where                   
                         removeElem (f,h:s) = f ++ s
                         removeElem ([],h:s) =  s
                         removeElem ([],h) =  []
                         removeElem (f,h) =  f


