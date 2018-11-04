square :: Num a => a -> a
square x = x * x

double x = x+x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

average1 ns = div (sum ns) (length ns)


faverage ns = sum ns / fromIntegral(length ns)

faverage1 ns = (/) (sum ns) (realToFrac(length ns))


n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last' xs = drop (length xs -1) xs !! 0

last'' xs = head (reverse xs)

last''' xs = head( drop (length xs -1) xs )

last'''' xs = xs !! (length xs - 1)

init' xs = take (length xs - 1) xs

init'' xs = reverse( tail(reverse xs))