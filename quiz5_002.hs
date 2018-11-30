--q1
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

--q2 
addInv :: [(Int,Int)]
addInv = [(x,y) | x<-[0..], let y = -x]

addInv' :: [(Int,Int)]
addInv' = [(x,-x) | x<-[0..]]

addInv'' :: [(Int,Int)]
addInv'' = [ p | p <- zip [0..] [0,-1..]]

--q3
repeat' x = [x] ++ repeat' x

repeat'' x = x : repeat'' x

--q4
pow :: Num a => a -> Int -> a
pow _ 0 = 1 -- pow x 1 = x
pow x p = x * pow x (p-1)