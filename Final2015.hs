mid xs = xs !!((length xs -1) `div` 2 )

trim'' xs= tail(reverse(tail(reverse xs)))
trim xs = (tail.init) xs
trim' = tail.reverse.tail.reverse

nand a b = not( a&&b)

nand' True True = False
nand' _ _       = True

getPrices (i:is) (p:ps) item
  | i == item = p
  | otherwise = getPrices is ps item

--max' [] = undefined
max' [x] = x
max' (x:xs)
  | x> maxxs = x
  | otherwise = maxxs
    where maxxs = max' xs

monoInc [x] = True
monoInc (x1:x2:xs)
  | x1 <= x2 = True && (monoInc (x2:xs))
  | otherwise = False

pairs xs = zip xs [xs!!i|i<-[1..(length xs -1)]]

pairs' xs = zip xs (tail xs)

