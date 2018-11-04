triangleArea :: Fractional a => a -> a -> a
triangleArea b h = (b*h)/ 2

firstHalf :: [a] -> [a]
firstHalf xs = take (length xs `div` 2) xs

firstHalf' xs = take half xs
    where
        half = length xs `div` 2

secondHalf xs = drop (length xs `div` 2) xs

split :: [a] -> [[a]]
split xs = [ys,zs]
    where
        ys=firstHalf xs
        zs=secondHalf xs
