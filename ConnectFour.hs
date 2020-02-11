import System.IO

-- Used to display the board in an easy to udnerstand way
showBoard :: [[Char]] -> [Char]
showBoard [a,b,c,d,e,f] = " 0 1 2 3 4 5 6 \n" ++
    createRow a ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow b ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow c ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow d ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow e ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow f ++ "\n──┴─┴─┴─┴─┴─┴──\n"


-- Creates an easy to see row for the board
createRow :: [Char] -> [Char]
createRow [a,b,c,d,e,f,g] = " "
    ++ [getSymbol a, '│', getSymbol b, '│', getSymbol c, '│', getSymbol d, '│', getSymbol e, '│', getSymbol f, '│', getSymbol g]
    

-- Returns the symbol if it is not an empty space, if it is, returns a space
getSymbol :: Char -> Char
getSymbol a = if a == '.' then ' ' else a


-- Returns the charcter of the winner if there is a winner, '-' if it is a tie, and '?' if there is no winner and the game is not over
checkWinner :: [[Char]] -> Char
checkWinner board = 
    let
        -- Checking diagonally
        check1 :: [[Char]] -> Int -> Int -> Char
        check1 board x y 
            | board!!y!!x == board!!(y+1)!!(x+1) && board!!(y+1)!!(x+1) == board!!(y+2)!!(x+2) && board!!(y+2)!!(x+2) == board!!(y+3)!!(x+3) && board!!y!!x /= '.' = board!!y!!x
            | board!!(y+3)!!x == board!!(y+2)!!(x+1) && board!!(y+2)!!(x+1) == board!!(y+1)!!(x+2) && board!!(y+1)!!(x+2) == board!!y!!(x+3) && board!!(y+3)!!x /= '.' = board!!(y+3)!!x
            | otherwise = '?'

        -- Checking vertically
        check2 :: [[Char]] -> Int -> Int -> Char
        check2 board x y
            | board!!y!!x == board!!(y+1)!!x && board!!(y+1)!!x == board!!(y+2)!!x && board!!(y+2)!!x == board!!(y+3)!!x && board!!y!!x /= '.' = board!!y!!x
            | otherwise = '?'
        
        -- Checking horizontally
        check3 :: [[Char]] -> Int -> Int -> Char
        check3 board x y
            | board!!y!!x == board!!y!!(x+1) && board!!y!!(x+1) == board!!y!!(x+2) && board!!y!!(x+2) == board!!y!!(x+3) && board!!y!!x /= '.' = board!!y!!x
            | otherwise ='?'

        -- Filter out all the checks with no winner
        wins = filter (/='?') [     
            -- Checking all possible diagonals
            check1 board 0 0, check1 board 0 1, check1 board 0 2,
            check1 board 1 0, check1 board 1 1, check1 board 1 2,
            check1 board 2 0, check1 board 2 1, check1 board 2 2,
            check1 board 3 0, check1 board 3 1, check1 board 3 2,
            -- Checking all the possible vericals
            check2 board 0 0, check2 board 0 1, check2 board 0 2,
            check2 board 1 0, check2 board 1 1, check2 board 1 2,
            check2 board 2 0, check2 board 2 1, check2 board 2 2,
            check2 board 3 0, check2 board 3 1, check2 board 3 2,
            -- Checking all the possible horizontals
            check3 board 0 0, check3 board 0 1, check3 board 0 3, check3 board 0 4, check3 board 0 5,
            check3 board 1 0, check3 board 1 1, check3 board 1 3, check3 board 1 4, check3 board 1 5,
            check3 board 2 0, check3 board 2 1, check3 board 2 3, check3 board 2 4, check3 board 2 5,
            check3 board 3 0, check3 board 3 1, check3 board 3 3, check3 board 3 4, check3 board 3 5
            ]        
    in 
        -- There will only be one winner but a player can win two ways in one move, so all the wins will be the same
        if length wins /= 0 then wins!!0
        -- If there is a free space (marked by a '.'), the game is not over, if there is not, it is a tie
        else if '.' `elem` board!!0 ++ board!!1 ++ board!!2 ++ board!!3 ++ board!!4 ++ board!!5 then '?' else '-'


-- Given a column, returns if the player can play, and if so, at which y position
checkPlay :: [[Char]] -> Int -> Int -> (Bool, Int)
checkPlay board x y
    -- If x is too big, return False
    | x > length board = (False, -1)
    -- If x is less than zero, return False
    | x < 0 = (False, -1)
    -- If y is 0 (the top of the board), return if the space is free
    | y == 0 = (board!!y!!x == '.', 0)
    -- If the space being checked is empty (marked by a '.'), return the move as valid and the y value of the free space
    | board!!y!!x == '.' = (True, y)
    -- If the space is not free, check the space above
    | otherwise = checkPlay board x (y-1)

-- Replaces character in position x y with the character a
play :: Int -> Int -> a -> [[a]] -> [[a]]
play x y a board = replace y (replace x a (board !! y)) board


-- Replaces element in position n with a in an array
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a : xs
replace n a (x:xs) = x : (replace (n-1) a xs)


-- The code logic for playing the game
twoPlayer :: [[Char]] -> Char -> IO()
twoPlayer board player
    -- Checks for the game being over
    | winner == '-' = putStrLn ((showBoard board) ++ "\nIt's a tie!")
    | winner /= '?' = putStrLn ((showBoard board) ++ "\n" ++ [winner] ++ " wins the game!")        
    -- Game is not over, so play on
    | otherwise = do
        putStrLn (showBoard board)
        putStr ("Player '" ++ [player] ++ "' enter a column to play: ")
        hFlush stdout
        input <- getLine
        let 
            x = (read input::Int)
            -- Checking if the move is valid
            (valid, y) = checkPlay board x ((length board)-1)
        if valid then
            -- Placing the players counter
            let board2 = (play x y player board)
            -- Seeing who goes next
            in if (player == 'O') then (twoPlayer board2 'X') else (twoPlayer board2 'O')
        else do
            -- Move is invalid
            putStrLn "\nMove is invalid!"
            -- Same player to play, with no changes to the board
            twoPlayer board player
    where winner = checkWinner board


emptyBoard = [
    ".......",
    ".......",
    ".......",
    ".......",
    ".......",
    "......."]


main :: IO()
main = do
    twoPlayer emptyBoard 'X'