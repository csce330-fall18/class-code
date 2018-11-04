and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

repl' :: Int -> a -> [a]
repl' 0 _ = []
repl' n x = x : repl' (n-1) x


(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = xs Main.!! (n-1)   


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x==y = True
    | otherwise = elem' x ys



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x<=y = x: merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
{-
msort :: Ord a => [a] -> [a]


-}