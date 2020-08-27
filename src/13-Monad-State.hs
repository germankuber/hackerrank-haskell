import Control.Monad.State
import Data.Monoid
import Control.Monad.Writer

type Stack = [Int]
  
push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

popM :: State Stack Int
popM = state $ \(x:xs) -> (x,xs)



stackManip :: State Stack Int
stackManip = do
    push 3
    popM
    popM


stackStuff :: State Stack ()
stackStuff = do
    a <- popM
    if a == 5
        then push a
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()


stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]





quitar :: State [a] a
quitar = state $ \(x:xs) -> ( x, xs)

agregar :: a -> State [a] a
agregar n = state $ \xs -> (n, n:xs)




primeraMaybe :: Int -> Maybe Int
primeraMaybe n | odd n = Just n
               | otherwise = Nothing
        
segundaEither :: Int -> Either String Int
segundaEither n | mod n 5 == 0 = Right n
                | otherwise = Left "No es multiplo de 5"


terceroIO :: Int -> IO Int
terceroIO = return

probar :: Int -> IO String
probar n = return $ show n
test n = do
    primeraMaybe
    segundaEither
    terceroIO
    probar
    
dame:: IO String
dame = return ""

loopPlus :: State [Int] Int
loopPlus = do
    array <- get
    if head array < 5
        then do
            put [10]
            loopPlus 
        else return $ head array

        
-- type Birds = Int
-- type Pole = (Birds,Birds)

-- landLeft :: Birds -> Pole -> Maybe Pole
-- landLeft n (left,right)
--     | abs ((left + n) - right) < 4 = Just (left + n, right)
--     | otherwise                    = Nothing

-- landRight :: Birds -> Pole -> Maybe Pole
-- landRight n (left,right)
--     | abs (left - (right + n)) < 4 = Just (left, right + n)
--     | otherwise                    = Nothing



-- landLeftE :: Birds -> Pole -> Either String Pole
-- landLeftE n (left,right)
--     | abs ((left + n) - right) < 4 = Right (left + n, right)
--     | otherwise                    = Left "Se cae por Izquierda"

-- landRightE :: Birds -> Pole -> Either String Pole
-- landRightE n (left,right)
--     | abs (left - (right + n)) < 4 = Right (left, right + n)
--     | otherwise                    = Left "Se cae por derecha"

type KnightPos = (Int,Int)
moveKnight ::  Int -> [KnightPos] -> ([KnightPos],[KnightPos])
moveKnight  v s= do
    let filtered ((c,r):xs)  =   filtered xs ++ filter onBoardStep  [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
        filtered []          = []
        onBoardStep (a,b) =  a `elem` [1..8] && b `elem` [1..8]
        solution = filtered s
     in (solution, s++ solution)



tryMove :: Int -> [KnightPos] -> [KnightPos]
tryMove v s= let  (a,b) = moveKnight v s
             in if not (null a)
                then  fst $ moveKnight v b
                else  b
