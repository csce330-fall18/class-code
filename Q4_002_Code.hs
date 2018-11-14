alg :: Num a => a -> a -> a -> [a]
alg x y z = [x+y,x+z]

alg2 :: Eq a => a -> a -> a -> Bool
alg2 x y z = x == y || x == z

--Median: awful, just plain awful 
--type not required

median x y z 
    | x >= y && x <=z || x <= y && x >= z = x
    | y >= x && y <=z || y <= x && y >= z = y
    | otherwise  = z 

--short version: because the symbol '-' is used for both subtraction and the unary minus
-- sections using the minus sign are tricky, this answer works but is silly 
pred :: Num a => a -> a
pred x = (-) x 1

