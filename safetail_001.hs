null' :: [a] -> Bool
null' [] = True
null' _ = False

null'' xs = if xs == [] then True else False

null''' xs 
    | xs == [] = True
    | otherwise = False

safetail xs = if null' xs then [] else tail xs 

safetail' xs 
    | null' xs = []
    | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs 