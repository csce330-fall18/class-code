type Board = [Int]
type Player = String
type Players = (Player,Player)

nl :: IO ()
nl = putChar '\n'

drawBoard :: Board -> IO ()
drawBoard b = 
    do
        putStrLn $ concat [(show i)++":"++ concat (take c $ repeat " *" ) ++ "\n"| (c,i)<-cis ]
        --putStrLn $show board
    where
      cis = zip b [1..]

validPlay :: Board -> Int -> Int -> Bool
validPlay b pile ct=
        if pile > 0 && pile <= length b then
            if b !! (pile-1) >= ct then
                True
            else
                False
        else
            False

makePlay :: Board -> Int -> Int -> Board
makePlay b pile ct= [ if i==pile-1 then b !! i - ct else b !! i  | i<-[0..length b -1]]

other:: (Player,Player) -> Player -> Player
other (a,b) x 
    | x==a = b
    | otherwise = a

gameLoop :: Board -> Players -> Player -> IO()
gameLoop brd players active =
  do
    if (sum brd == 0)
        then
            do
                nl
                putStrLn $ "Congrats " ++ other players active ++ "!!"
    else
        do
            nl
            drawBoard brd
            putStrLn $ active ++ "'s move..."
            putStr "what pile?: "
            sPile <- getLine
            putStr "how many?: "
            sCt <- getLine
            let pile = read sPile :: Int
            let ct = read sCt :: Int
            if validPlay brd pile ct 
                then
                    do
                        gameLoop (makePlay brd pile ct) players (other players active) 
            else
                do gameLoop brd players active --try again
    



nimGame:: IO ()
nimGame = do
    putStr "Player1 Name: "
    p1<-getLine
    putChar '\n'
    putStr "Player2 Name: "
    p2<-getLine
    putStr "Enter the board: "
    strBoard <-getLine
    let board = map read $ words strBoard :: [Int] -- https://stackoverflow.com/questions/8879391/how-do-i-convert-string-into-list-of-integers-in-haskell
    gameLoop board (p1,p2) p1
