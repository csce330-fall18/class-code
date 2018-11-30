data Choice = Rock | Paper | Scissors deriving Show

beats :: Choice -> Choice -> Bool
beats Rock Scissors  = True
beats Scissors Paper = True
beats Paper  Rock    = True
beats _ _ = False

isTxtFile :: String -> Bool
isTxtFile fn = eliFtxTsi $ reverse fn
    
eliFtxTsi ('t':'x':'t':'.':_) = True
eliFtxTsi _ = False

isTxtFile' :: String -> Bool
isTxtFile' fn 
    | l4 == ".txt" = True
    | otherwise = False
    where
        l4 = drop  (length fn - 4) fn
        
--isTxtFile _ = False

isRepeat3 [] =False
isRepeat3 [_] =False
isRepeat3 [_,_] = False
isRepeat3 [_,_,_] =True
isRepeat3 [_,_,_,_] =False
isRepeat3 [_,_,_,_,_] =False
isRepeat3 (x1:y1:z1:x2:y2:z2:tail)
        | x1==x2 && y1==y2 && z1==z2 = isRepeat3 (x2:y2:z2:tail)
        | otherwise = False 

isRepeat3' xs 
    | length xs `mod` 3 > 0 = False
    | allRepeats $ trips xs = True
    | otherwise = False

allRepeats [x] = True
allRepeats (x:y:rest) 
    | x== y = allRepeats(y:rest)
    | otherwise = False

trips [] = []
trips (x:y:z:rest) = (x,y,z): trips rest

notTooSteep :: (Num a,Ord a) => [a] -> Bool
notTooSteep [] = True
notTooSteep [_] = True
notTooSteep (x:y:rest) 
    | d <= 1 && d >= (-1) = notTooSteep (y:rest)
    | otherwise = False
    where 
        d = x -y

b2v :: String -> String
b2v s = [ if l == 'b' then 'v' else l | l<-s]

countOnes :: String -> Int
countOnes bs= length [ b| b<-bs, b=='1']

countOnes' :: String -> Int
countOnes' bs= sum [ 1| b<-bs, b=='1']

all' :: (a->Bool) -> [a] -> Bool
all' p xs =  and $ map p xs

all'' :: (a->Bool) -> [a] -> Bool
all'' p xs = length xs == (length $ filter p xs)

all''' :: (a->Bool) -> [a] -> Bool
all''' p xs = foldr (\x y -> p x && y) True xs

sansNeg :: (Num a,Ord a) => [a] -> [a]
sansNeg xs = concat $ foldr (\x y -> (if x<0 then [] else [x] ): y) [] xs

sansNeg' :: (Num a,Ord a) => [a] -> [a]
sansNeg' xs = filter ( >=0) xs