nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))


data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n) 

mult:: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr deriving Show

folde vf af mf (Val n) =  vf n
folde vf af mf (Add m n) = af  (folde vf af mf m)  (folde vf af mf n)
folde vf af mf (Mul m n) = mf ( folde vf af mf m) (folde vf af mf n) 

data Tree a = Nil 
            | Leaf a
            | Node (Tree a) a (Tree a) deriving Show

size :: Tree a -> Int
size Nil = 0
size (Leaf _) = 1
size (Node l _ r ) = size l + 1 + size r

complete :: Tree a -> Bool
complete Nil = True
complete (Leaf _) = True
complete (Node l _ r) = size l == size r && complete l && complete r












