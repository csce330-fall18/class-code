pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[y..n],x^2+y^2 == z^2]


factors :: Int -> [Int]
factors n =
   [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

perfects :: Int -> [Int]
--perfects n = [x | x <-[1..n], sum (init $ factors x) == x  ]
perfects n = [x | x <-[1..n], (sum.init.factors) x == x  ]



sp :: Num a => [a] -> [a] -> a
sp xs ys = sum [ xs!!i * ys!!i | i <-[0..length xs-1]]


sp' :: Num a => [a] -> [a] -> a
sp' xs ys = sum [ fst xy * snd xy | xy<-xys]
    where
        xys = zip xs ys

sp'' :: Num a => [a] -> [a] -> a
sp'' xs ys = sum [ fst xy * snd xy | xy<-(zip xs ys)]



fourthToLast :: [a] -> a
fourthToLast xs = last (init (init (init xs)))

fourthToLast' :: [a] -> a
fourthToLast' xs = reverse xs !! 3

fourthToLast'' :: [a] -> a
fourthToLast'' xs = xs !! (length xs - 4)


noEnds :: [a] -> [a]
noEnds xs = tail $ init xs

noEnds' :: [a] -> [a]
noEnds' = tail.init