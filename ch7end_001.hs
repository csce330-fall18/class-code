filtmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap p f xs = map f (filter p xs)

--with function composition
filtmap' :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
filtmap' p f = (map f ).(filter p)

--with foldr
map' :: (a->b) -> [a] -> [b] 
map' f = foldr (\x y-> f x : y) [] 

--with a l.c.
map'' :: (a->b) -> [a] -> [b] 
map'' f xs = [ f x | x<- xs]

--with foldl
map''' :: (a->b) -> [a] -> [b] 
map''' f xs = foldl (\ x y -> x ++ [f y]) [] xs
{-
--foldr with conditional
filter' :: (a->Bool) -> [a] -> [a]


--with a l.c.
filter'' :: (a->Bool) -> [a] -> [a]

-}