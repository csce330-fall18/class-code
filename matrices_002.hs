type Matrix = [[Double]] -- row, column

type Vector = [Double]

ip :: Vector -> Vector -> Double
ip v1 v2 = sum $ map (\ tup -> fst tup * snd tup ) (zip  v1 v2)


validip :: Vector->Vector->Bool
validip v1 v2 = length v1 == length v2

row :: Matrix -> Int -> Vector
row mat m = mat !! m

col :: Matrix -> Int -> Vector
col mat n = [ r !! n | r<-mat]
{-
n_rows :: Matrix -> Int

n_cols :: Matrix -> Int

validmmul :: Matrix -> Matrix -> Bool

mmul :: Matrix -> Matrix -> Matrix

print_matrix :: Matrix -> IO ()
-}


m3x3 :: Matrix
m3x3 = [ [1,2,3],[2,1,4],[5,2,1]]
    