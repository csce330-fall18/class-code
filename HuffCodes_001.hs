import Data.List

data HTree = CNode Double Char
           | INode Double HTree HTree deriving Show

freq :: HTree -> Double

--remind to parenthesize parameter

merge :: HTree -> HTree -> HTree

mergeAllTrees :: [HTree]  -> HTree 


buildHTree :: [(Char,Double)] -> HTree 


decodeTree :: String -> HTree -> [(Char,String)]


--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]

getCodes' :: [(Char,Double)] -> [(Char,String)]

get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        