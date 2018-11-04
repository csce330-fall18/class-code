(||) :: Bool -> Bool -> Bool
{-
True  || True = True
True  || False = True
False || True = True
False || False = False
-}
{-
False  || False = False
_      ||   _   = True
-}
False || b = b
True  || _ = True

(&&) :: Bool -> Bool -> Bool
--(&&) x y = if x then (if y then True else False) else False
(&&) x y = if x then y else False
