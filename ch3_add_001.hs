add :: (Int,Int) -> Int
add (x,y) = x+y

add' :: Int -> (Int -> Int)
add' x y = x+y

addNAll :: Int -> [Int]-> [Int]
addNAll n ns = map (add' n) ns

addNAll' :: Int -> [Int]-> [Int]
addNAll' n ns = map ((+) n) ns

listEvenLength :: [a] -> Bool
listEvenLength [] = True
listEvenLength [_] = False
listEvenLength xs = listEvenLength $ drop 2 xs