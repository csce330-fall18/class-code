type Matrix = [[Double]] -- row, column

type Vector = [Double]

ip :: Vector -> Vector -> Double
ip v1 v2 = sum $ map (\(x,y)->x*y) $ zip v1 v2

validip :: Vector->Vector->Bool
validip v1 v2 = length v1 == length v2

row :: Matrix -> Int -> Vector
row mat r = mat !! r

col :: Matrix -> Int -> Vector
col [] _ = []
col (r:rs) c = r !! c : col rs c

n_rows :: Matrix -> Int
n_rows m = length  m 

n_cols :: Matrix -> Int
n_cols m = length $ m !! 0 

validmmul :: Matrix -> Matrix -> Bool
validmmul a b = n_cols a == n_rows b

mmul :: Matrix -> Matrix -> Matrix
mmul a b = [ [ ip (row a i) ( col b j ) | j<-[0..n_rows b -1] ] | i <-[0..n_rows a-1]  ]

print_matrix :: Matrix -> IO ()
print_matrix [] = return ()
print_matrix (r:rs) = 
  do 
    putStrLn $show r
    print_matrix rs



m3x3 :: Matrix
m3x3 = [ [1,2,3],[2,1,4],[5,2,1]]
    