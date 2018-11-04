mult x y z = x*y*z

add'::Num a => a -> a -> a
add' x y  = x+y

add1::Num a => a -> a 
add1 x = x+1

add1'::Num a => a -> a 
add1' x = add' 1 x

add1''::Num a => a -> a 
add1'' = add' 1