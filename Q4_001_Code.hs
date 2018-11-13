
alg :: Num a => (a,a,a) -> a
alg (x,y,z) = x+y+z

alg2 :: a-> a-> a-> [a]
alg2 x y z = [x,y,z]


fatty p c f 
    | (4* (p+c)) / (9 *f)  >= 2 = False
    | otherwise = True


half = (/2)

half' = (*0.5)
