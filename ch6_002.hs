and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
--concat' [xs] = xs 
concat' (xs:xss) = xs ++ concat' xss

repl' :: Int -> a -> [a]
repl' 0 _ = []
repl' n x = x: repl' (n-1) x

--(!!) :: [a] -> Int -> a
--(!!) (x:_) 0 = x
--(!!) (_:xs) n =  xs Main.!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
    | x==y = True
    | otherwise = elem' x ys

elem'' x ys = or [ x==y | y<-ys]

elem''' x ys = length [ 1 | y<-ys, x==y  ] > 0

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x<=y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        half = length xs `div` 2
        left = take half xs
        right = drop half xs

fourthToLast :: [a] -> a
fourthToLast xs = reverse xs Prelude.!! 3

fourthToLast' :: [a] -> a
fourthToLast' xs = xs Prelude.!! (length xs -4 )

fourthToLast'' :: [a] -> a
fourthToLast'' xs = head $ drop (length xs -4) xs

fourthToLast''' :: [a] -> a
fourthToLast''' xs = last $ take 4 (reverse xs)

noEnds :: [a] -> [a]
noEnds xs = reverse(tail( reverse (tail xs) ) )

noEnds' :: [a] -> [a]
noEnds' xs = [xs !! i | i <- [1..length xs -2]]

noEnds'' :: [a] -> [a]
noEnds'' xs = tail $ init xs

noEnds''' :: [a] -> [a]
noEnds''' xs = (tail.init) xs

noEnds'''' :: [a] -> [a]
noEnds''''  = tail.init