filtmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap ffilt fmap xs = (map fmap)(filter ffilt xs)

--with function composition
filtmap' :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
filtmap' ffilt fmap = (map fmap).(filter ffilt)

--with foldr
map' :: (a->b) -> [a] -> [b] 
map' f xs = foldr (\x y -> (f x):y) [] xs

--with a l.c.
map'' :: (a->b) -> [a] -> [b] 
map'' f xs = [ f x | x<-xs ]

--with foldl
map''' :: (a->b) -> [a] -> [b] 
map''' f xs = foldl (\x y -> x ++ [f y]) [] xs

--foldr with conditional
filter' :: (a->Bool) -> [a] -> [a]
filter' p xs = foldr (\x y ->  if (p x) then x:y else y) [] xs

--with a l.c.
filter'' :: (a->Bool) -> [a] -> [a]
filter'' p xs = [x | x<-xs, p x]