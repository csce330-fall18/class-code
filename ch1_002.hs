-- higher order functions examples... will do many
-- technically, a higher order function is a function that 
-- takes OR returns a function
-- but most functions that take 2+ args return functions 
-- as intermediate values (currying) so we 
-- reserve "higher order" in Haskell for functions
-- that take functions as arguments

-- working recursively

sum' [] = 0
sum' (x:xs) = x+ (sum' xs)


-- using a Higher order function
sum'' xs = foldl (+) 0 xs

add x y = x+y
sum'''' xs = foldl add 0 xs

--or even just
sum''' :: Num a => [a] -> a
sum''' = foldl add 0


