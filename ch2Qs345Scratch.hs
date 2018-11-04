{- our "menu"
 head, tail, !!, take, drop, length, sum, product, ++, reverse
-}

--Q3
last' xs = xs !! length xs - 1

--Q4
last'' xs = head (reverse xs)

--Q4 yet another way
last'''' xs = head (drop (length xs - 1) xs)

--Q4 alt (with recursion)
last''' [x] = x
last''' xs = last''' (tail xs)

--Q5 - init
init' xs = take (length xs -1) xs

--q5 - alt
init'' xs = reverse (tail (reverse xs))

--q5 - recursion
init''' [x] = []
init''' (x:xs) = [x]++ init''' xs