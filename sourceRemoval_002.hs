
data Vertx a = Vertx a [a] deriving Show 

type Graph a = [Vertx a] 

label (Vertx l _) = l

neighbors (Vertx _ ns) = ns

labelList::Graph a -> [a]
labelList [] = []
labelList (v:vs) = ls ++ labelList vs
    where
        ls = [label v] ++ neighbors v


uniq::Eq a=> [a] -> [a]
uniq [] = [] 
uniq (l:ls)
        | elem l ls = uniq ls
        | otherwise = l : uniq ls


allLabels:: Eq a=> Graph a -> [a]
allLabels = (uniq.labelList) 


--topo. sort w/source removal

sourceRemoval :: Eq a => Graph a ->([a],[a])
sourceRemoval [] = ([],[])
sourceRemoval g 
    | nodep == [] = ([],labelList purged)
    | otherwise = (nodep ++ indep, dep)
    where
        ls = allLabels g
        nodep = [l | l<-ls, noIncoming l g]
        purged = filter (\v -> not ( elem (label v) nodep)) g
        (indep, dep )= sourceRemoval purged 



noIncoming::Eq a=> a -> Graph a -> Bool
noIncoming _ [] = True
noIncoming l ((Vertx _ ns) : vs)
        | elem l ns = False
        | otherwise = noIncoming l vs


remove:: Eq a => a -> Graph a -> Graph a
remove l g = [v | v <-g , label v /= l] 

