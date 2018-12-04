

{-
Q1. Give a function that uses a list comprehension, primarily, that takes a string to return a string with all the spaces, ‘ ‘ removed. You may use a helper function or not equals, /= .  
-}
noSpaces :: String -> String
noSpaces s = [ l | l<-s, l /= ' ' ]

--filter version (not list comprehension)
noSpaces' :: String -> String
noSpaces' = filter (\x -> x/=' ') 

{-
Q2. Define repeat’ with a list comprehension, a function which takes one argument and returns an infinite list of the argument repeated.
-}
repeat' :: a -> [a]
repeat' x = [ x | i<-[0..]]

{-
Q3. Define a freeOf function that takes a list and a value and returns true if the list does not contain the item. Your definition must be recursive.
freeOf :: Eq a => [a] ->a ->Bool 
-}
freeOf :: Eq a => [a] -> a -> Bool
freeOf [] _ = True
freeOf (x:xs) y 
    | x == y = False
    | otherwise = freeOf xs y

{-
Q4.  Define a recursive isPowerOf function that returns true if the first argument is an (integral) power of the second argument. You may assume all arguments are positive. 27, 81 are powers of 3 but 30, which is just a multiple, is not.
-}
isPowerOf ::Integral a => a -> a-> Bool
isPowerOf 1 b = True
isPowerOf p b 
    | p `mod` b ==0 = isPowerOf (p `div`b) b
    | otherwise = False