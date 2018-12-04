--q1 Give a function that uses a list comprehension, primarily,  to return a string without its vowels. You may use elem or a helper function. You may assume the input string has no uppercase letters. 
noVowels :: String -> String
noVowels s = [ l | l<-s, not $ elem l "aeiou"]  

vowel :: Char -> Bool
vowel 'a' = True
vowel 'e' = True
vowel 'i' = True
vowel 'o' = True
vowel 'u' = True
vowel _ = False

noVowels' s = [ l | l<-s, not $ vowel l ]

--q2 Give the list comprehension for all the integers as tuples that are additive inverses of one another in (positive, negative order), i.e.
--[(0,0),(1,-1),(2,-2),(3,-3),(4,-4),(5,-5),(6,-6),(7,-7),(8,-8),… ]

addInv :: [(Int,Int)]
addInv = [(x,y) | x<-[0..], let y = -x]

addInv' :: [(Int,Int)]
addInv' = [(x,-x) | x<-[0..]]

addInv'' :: [(Int,Int)]
addInv'' = [ p | p <- zip [0..] [0,-1..]]

--q3 Define repeat’, recursively, a function which takes one argument and returns an infinite list of the argument repeated, e.g. (Hint: What’s the base case, if any?)
-- take 4 $ repeat’ 3 returns [3,3,3,3]

repeat' x = [x] ++ repeat' x

repeat'' x = x : repeat'' x

--q4 Define a recursive isPowerOf function that returns true if the first argument is an (integral) power of the second argument. You may assume all arguments are positive. 27, 81 are powers of 3 but 30, which is just a multiple, is not.
--  isPowerOf :: Integral a => a -> a -> Bool
--  isPowerOf 1 b = True -- hint: here’s the base case

pow :: Num a => a -> Int -> a
pow _ 0 = 1 -- pow x 1 = x
pow x p = x * pow x (p-1)