import Data.List

data HTree = CNode Double Char
           | INode Double HTree HTree deriving Show

freq :: HTree -> Double
freq (CNode f _) = f
freq (INode f _ _) = f

--remind to parenthesize parameter

merge :: HTree -> HTree -> HTree
merge x y = INode ( freq x + freq y ) x y


mergeAllTrees :: [HTree]  -> HTree 
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t:rest)
    where
        (f:s:rest) = sortOn freq ts
        t = merge f s

buildHTree :: [(Char,Double)] -> HTree 
buildHTree charfreqs = mergeAllTrees cnodes
        where
            cnodes = map (\(ch,fr)->CNode fr ch) charfreqs

decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ left right) = left_codes ++ right_codes
    where
        left_codes = decodeTree ( pre ++ "0" ) left
        right_codes = decodeTree ( pre ++ "1" ) right

--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = sortOn fst codes
    where 
        t= buildHTree cfs
        codes = decodeTree "" t


getCodes' :: [(Char,Double)] -> [(Char,String)]
getCodes' cfs = sortOn fst (decodeTree "" $ buildHTree cfs)


get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        