{- our "menu"
 head, tail, !!, take, drop, length, sum, product, ++, reverse
-}

--Q3
last' xs = head (drop (length xs - 1) xs)
last'' xs = head $ reverse xs
last''' xs = xs !! (length xs -1)


last'''' [x] = x
last'''' (x:xs) = last'''' xs
--Q4
init' xs = take (length xs -1) xs

--Q4 yet another way
init'' xs = reverse (tail (reverse xs))

--Q4 alt (with recursion)
init''' [x] = []
init''' (x:xs) = [x] ++ init''' xs

--Q5 - init


--q5 - alt


--q5 - recursion
