type Board = [Int]
type Player = String
type Players = (Player,Player)

nl :: IO ()
nl = putChar '\n'

drawBoard :: Board -> IO ()
drawBoard b = db b 1

db :: Board -> Int -> IO ()
db [] _ = return ()
db (c:cs) i =
    do
        putStrLn $ (show i) ++ ": " ++ concat [ " *" | x<-[1..c] ]
        db cs (i+1)

other:: Players -> Player -> Player
other (a,b) x 
    | x==a = b
    | otherwise = a


validPlay :: Board -> Int -> Int -> Bool
validPlay b row ct =
    if (row > 0 && row <= length b) && ct > 0 then
        if b !! (row -1) >= ct then
            True
        else
            False
    else 
        False

makePlay :: Board -> Int -> Int -> Board
makePlay b row ct = [ if i==row then c-ct else c | (c,i)<-cis]
    where
        cis = zip b [1..]

nimGame :: IO ()
nimGame = 
  do
    putStr "Player 1 name: "
    p1<-getLine
    putStr "Player 2 name: "
    p2<-getLine
    putStr "Enter the board: "
    strBoard<-getLine
    let board = map read $ words strBoard :: [Int]
    gameLoop board (p1,p2) p1

gameLoop :: Board -> Players -> Player -> IO () 
gameLoop brd players active=
    do
      if (sum brd == 0) then
        do
          nl
          putStrLn $ "Congrats " ++ other players active ++ "!!!"
      else
        do
          nl
          putStrLn $ "your turn " ++ active
          drawBoard brd
          putStr "what row?"
          sRow <- getLine
          putStr "how many?"
          sCt <- getLine
          let row = read sRow :: Int
          let ct = read sCt :: Int
          if validPlay brd row ct then
             do
                let newBoard = makePlay brd row ct
                gameLoop newBoard players $other players active
          else
            gameLoop brd players active


