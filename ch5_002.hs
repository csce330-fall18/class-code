pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1..n],y<-[x..n],z<-[y..n], z^2 == x^2 +y^2 ]

factors :: Int -> [Int]
factors n =
   [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

perfects :: Int -> [Int]
--perfects n = [ x | x<-[1..n], sum( init $ factors x) == x ]
perfects n = [ x | x<-[1..n], (sum.init.factors) x == x ]

sp :: Num a => [a] -> [a] -> a
sp as bs = sum [ as!!i * bs!!i | i <-[0..(length as -1)] ]

sp' :: Num a => [a] -> [a] -> a
sp' as bs = sum [ as!!i * bs!!i | i <-[0..(length as -1)] ]