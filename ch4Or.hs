(||) :: Bool -> Bool -> Bool
{-False || False = False
False || True = True
True || False = True
True || True = True -}
False || False = False
_ || _ = True


(&&) ::  Bool -> Bool -> Bool
---(&&) a b = if a then if b then True else False else False
(&&) a b = if a then b else False