add :: (Int,Int) -> Int
add (x,y) = x+y

add' :: Int -> (Int -> Int)
add' x y = x+y

listEvenLength :: [a] -> Bool
listEvenLength [] = True
listEvenLength [_] = False
listEvenLength xs = listEvenLength (drop 2 xs)