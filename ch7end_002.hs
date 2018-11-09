filtmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap p f xs  = (map f) (filter p xs)


--with function composition
filtmap' :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
filtmap' p f = (map f).(filter p)


--with foldr
map' :: (a->b) -> [a] -> [b] 
map' f xs = foldr (\x y -> f x : y) [] xs


--with a l.c.
map'' :: (a->b) -> [a] -> [b] 
map'' f xs = [ f x |x<-xs]


--with foldl
map''' :: (a->b) -> [a] -> [b] 
map''' f xs = foldl (\x y -> x ++ [f y] ) [] xs


--foldr with conditional
filter' :: (a->Bool) -> [a] -> [a]
filter' p xs = foldr (\x y-> if p x then x:y else y) [] xs


--with a l.c.
filter'' :: (a->Bool) -> [a] -> [a]
filter'' p xs = [ x | x<-xs, p x]

--Q4 (chs 3,4)
alg :: Num a => a -> a-> a-> [a]
alg x y z = [x+y,y+z]

alg1 :: Eq a => a-> a-> a-> Bool 
alg1 x y z = x == y || x==z

median :: Ord a => a -> a -> a -> a
median x y z 
   | x >= y && x <= z = x
   | z >= y && z <= x = z
   | otherwise = y

--pred' :: Num a => ()