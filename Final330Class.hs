mid xs = xs !! ((length xs -1 ) `div` 2)

trim xs = tail ( take (length xs-1) xs) 
trim' xs = tail(reverse(tail(reverse xs)))
trim'' xs = (tail.reverse.tail.reverse) xs

nand True True = False
nand _ _          = True

nand' a b = not ( a&&b ) 

getPrice [] [] _ = -1
getPrice (i:is) (p:ps) item
  | i==item = p
  |otherwise = getPrice is ps item
  
oddSquares = [x^2|x<-[1,3..] ]

max' [x] = x
max' (x:xs)
  | x > (max' xs) = x
  | otherwise = max' xs
  
max'' [x] = x
max'' (x:xs)
  | x > maxxs =x
  | otherwise = maxxs
  where maxxs = max'' xs

monoInc [x] = True
monoInc (x:y:xs) 
  | y < x = False
  | otherwise = monoInc (y:xs)
  
pairs xs = zip xs (tail xs)

--pairs' xs = [(x,y)|i<-[1..length xs] , let x=(xs !! (i-1)), let y=(xs!!i)]
pairs' xs = [(x,y)|i<-[1..(length xs-1)] , let x=(xs !! (i-1)), let y=(xs!!i)]

monoInc' xs = foldr  (&&) True  (map ( \(x,y) -> x <=y ) ps)
  where ps = pairs xs
  
revRev xs = reverse (map reverse xs)