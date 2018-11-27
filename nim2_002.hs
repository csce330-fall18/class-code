type Board = [Int]
type Player = String
type Players = (Player,Player)

nl :: IO ()
nl = putChar '\n'

other :: Players -> Player -> Player
other ps a = if fst ps == a then snd ps else fst ps

drawBoard :: Board -> IO ()
drawBoard b = 
  do
    putStrLn $ concat [ (show $snd ci)++": "++concat (take (fst ci) ( repeat " *")) ++ "\n"| ci<-cis]
    where
        cis = zip b [1..]

validPlay :: Board -> Int -> Int -> Bool
validPlay b pile ct =
    if pile >0 && pile <= length b && ct > 0 then
        if b !! (pile -1) >=ct then
            True
        else
            False
    else
        False

makePlay :: Board -> Int -> Int -> Board
makePlay b pile ct = [ if i==pile then c-ct else c | (c,i) <-cis]
        where 
            cis = zip b [1..]

gameLoop :: Board -> Players -> Player -> IO ()
gameLoop brd players active =
    do 
        if (sum brd == 0) then
            do
                nl
                putStrLn $ "Congrats " ++other players active ++"!!"
        else
            do
                nl
                drawBoard brd
                putStrLn $ active ++ "'s move..."
                putStr "what pile?: "
                sPile <- getLine
                putStr "how many?"
                sCt<-  getLine
                let pile = read sPile :: Int
                let ct = read sCt :: Int
                if validPlay brd pile ct then
                    do
                        let newBoard = makePlay brd pile ct
                        let nextPlayer  = other players active
                        gameLoop newBoard players nextPlayer
                else 
                    do
                        gameLoop brd players active --try again 


nimGame :: IO ()
nimGame = 
    do
      putStr "Player 1: "
      p1<-getLine
      putStr "Player 2: "
      p2<-getLine
      putStr "Enter the board: "
      strBoard <-getLine
      let board = map read $ words strBoard :: [Int]
      gameLoop board (p1,p2) p1