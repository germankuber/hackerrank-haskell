-- -- (+3) <$> Just 2

-- -- Just (+2) <*> Just 5

-- -- Just 3 >>= (\x -> Just (x + 10))

-- bastardoSinGloria :: [String]
-- bastardoSinGloria = ["omar", "oso", "raine"]

-- parGlorioso = [ (x, y) | x <- bastardoSinGloria, y <- bastardoSinGloria, x  /= y ]




-- raizCuadradaReal :: Float -> Either String Float
-- raizCuadradaReal a | a < 0 = Left "Error"   
--                    | otherwise = Right $ sqrt a


data Validated a= Valid a | Doubtful a String | Invalid String deriving (Eq, Show)

instance Functor Validated where
    fmap f (Valid b) = Valid ( f b)
    fmap f (Doubtful a e)= Doubtful (f a) e
    fmap f (Invalid a) = Invalid a 
    
instance Applicative Validated where
    pure x = Valid x
    (Valid f)  <*> (Valid b) = Valid (f b)
    (Valid f)  <*> (Doubtful c b) = Doubtful (f c) b
    (Invalid g) <*> _ = Invalid g
    _ <*> (Invalid g) = Invalid g
    


-- instance Monad Validated where
--     return a = Valid a
--     (Valid a)  >>= f = f a
--     (Doubtful a b)  >>= f = f a
--     (Invalid g) >>= _ = Invalid g

-- patrimonioAsquelCorp = Valid 2000000
-- empleadosNoditoInc :: Validated Integer
-- empleadosNoditoInc = Doubtful 45 "contratron mas gente"
-- tickerJemaRoja = Invalid "no está mas en la bolsa"


-- valorMensual :: Fractional a => Validated a -> Validated a
-- valorMensual a= mapValidated (/12) a


-- longitudTicker a = mapValidated length a

-- mapValidated :: Fractional a => (a -> b) -> Validated a -> Validated b
-- mapValidated f (Valid b) = Valid ( f b)
-- mapValidated f (Doubtful a e)= Doubtful (f a) e
-- mapValidated f (Invalid a) = Invalid a 


-- codigoUnico2 ::  Validated String ->  Validated String ->  Validated String
-- codigoUnico2 a b =  (\f g -> f ++ ":" ++ g)  <$>  a <*> b



-- codigoUnico :: String -> String -> String
-- codigoUnico a v = a ++ ":" ++ v

-- aplicarBinarioValidated f (Valid a) (Valid b) = Valid (f a b)
-- aplicarBinarioValidated f (Doubtful c d) (Valid b) = Doubtful (f c b) d
-- aplicarBinarioValidated f (Valid b) (Doubtful c d)  = Doubtful (f b c) d
-- aplicarBinarioValidated f (Doubtful a b) (Doubtful c d) = Doubtful (f a c) b
-- aplicarBinarioValidated _ (Invalid g) _ = Invalid g
-- aplicarBinarioValidated _ _ (Invalid g) = Invalid g


-- aplicarValidated :: Validated (a -> b) -> Validated a -> Validated b
-- aplicarValidated (Valid f) (Valid b) = Valid (f b)
-- aplicarValidated (Doubtful f d) (Valid b) = Doubtful (f b) d
-- aplicarValidated (Valid f) (Doubtful c d)  = Doubtful (f c) d
-- aplicarValidated (Doubtful f b) (Doubtful c d) = Doubtful (f c) b
-- aplicarValidated (Invalid g) _ = Invalid g
-- aplicarValidated _ (Invalid g) = Invalid g




-- validarIguales :: (Eq a, Show a) => a -> a -> Validated a
-- validarIguales a b | a == b = Valid a
--                    | otherwise = Invalid (show a ++ " /= " ++ show b)



-- validarIgualesValidated (Valid g) (Valid h) = validarIguales g h
-- validarIgualesValidated (Doubtful a b) (Doubtful c d) | a == c = Doubtful a b
-- validarIgualesValidated (Doubtful a b) (Valid c ) | a == c = Doubtful a b
-- validarIgualesValidated (Invalid g) _ = Invalid g
-- validarIgualesValidated _ (Invalid g) = Invalid g


-- bindValidated :: Validated a -> (a -> Validated b) -> Validated b
-- bindValidated (Valid a) f = f a
-- bindValidated (Invalid a) f = Invalid a


-- validarIgualesValidated :: Validated a -> Validated a -> Validated a
-- validarIgualesValidated x y = x >>= (\vx -> y >>= \vy -> validarIguales vx vy)


    
codigoUnico ::  Validated String ->  Validated String ->  Validated String
codigoUnico a b =  (\f g -> f ++ ":" ++ g)  <$>  a <*> b


