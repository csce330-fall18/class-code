nor :: Bool -> Bool -> Bool
nor False False = True
nor _ _         = False

addInv :: (Eq a, Num a) => a -> a -> Bool 
addInv x y
    | ( x + y) == 0 = True
    | otherwise = False

isPal :: String -> Bool
isPal [] = True
isPal [_] = True
isPal xs 
    | head xs == last xs = isPal ( tail $ init xs)
    | otherwise = False

isPal' xs = and [ xs!!i == revxs!!i | i <- [0.. length xs `div` 2] ]
    where
        revxs = reverse xs

isPal'' xs = and( map (\(x,y)-> x==y)    (zip xs $ reverse xs ) )

isPal''' xs = and( map (\tup-> fst tup == snd tup)    (zip xs $ reverse xs ) )


powersOf3 :: [Int]
powersOf3 = [ 3^p | p<-[0..] ] 

adjEq :: Eq a => [a] ->Bool
adjEq [] = False
adjEq [_] = False
adjEq (h1:h2:xs) 
    | h1 == h2 = True
    | otherwise = adjEq (h2:xs)

adjEq' xs = or ( map (\(x,y) -> x==y) $ zip xs (tail xs) )

l2Norm xs = sqrt (foldr (\x y -> x^2 +y) 0 xs) 

l2Norm' xs = sqrt ( sum $ map (^2) xs )

l1Norm xs = sum  xs 

validPoints xs ys = length xs == length ys

sub xs ys = map (\(x,y)->x-y) $ zip xs ys 

euc_dist p1 p2 = l2Norm $ sub p1 p2
