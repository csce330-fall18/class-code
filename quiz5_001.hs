

noSpaces :: String -> String
noSpaces s = [ l | l<-s, l /= ' ' ]

--filter version (not list comprehension)
noSpaces' :: String -> String
noSpaces' = filter (\x -> x/=' ') 

repeat' :: a -> [a]
repeat' x = [ x | i<-[0..]]

freeOf :: Eq a => [a] -> a -> Bool
freeOf [] _ = True
freeOf (x:xs) y 
    | x == y = False
    | otherwise = freeOf xs y

isPowerOf ::Integral a => a -> a-> Bool
isPowerOf 1 b = True
isPowerOf p b 
    | p `mod` b ==0 = isPowerOf (p `div`b) b
    | otherwise = False