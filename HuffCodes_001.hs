import Data.List

data HTree = CNode Double Char
           | INode Double HTree HTree deriving Show

freq :: HTree -> Double
freq (CNode f _) = f
freq (INode f _ _ ) =f
--remind to parenthesize parameter


merge :: HTree -> HTree -> HTree
merge c1 c2 = INode ( freq c1 + freq c2) c1 c2

mergeAllTrees :: [HTree]  -> HTree 
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (new_tree:rest)
    where
        (h1:h2:rest) = sortOn freq ts 
        new_tree = merge h1 h2


buildHTree :: [(Char,Double)] -> HTree 
buildHTree cfs = mergeAllTrees cnodes
    where
        cnodes = map (\(c,f) -> CNode f c) cfs 



decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ l r) = left_codes ++ right_codes
        where
            left_codes = decodeTree ( pre ++ "0" ) l 
            right_codes = decodeTree ( pre ++ "1" ) r 

--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = ccodes 
    where 
        unsorted = decodeTree "" $ buildHTree cfs
        ccodes = sortOn (\(c, _)-> c) unsorted

getCodes' :: [(Char,Double)] -> [(Char,String)]
getCodes' cfs = ccodes 
    where 
        unsorted = decodeTree "" $ buildHTree cfs
        ccodes = sortOn fst unsorted



get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        