import Data.Char (digitToInt, isDigit)

-- p.129 - p.132

{-
  1: * * * * *
  2: * * * *
  3: * * *
  4: * *
  5: *
-}

type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1
next _ = 0

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard board = do
  pb 1 board
  where
    pb _ [] = return ()
    pb i (b : bs) = do
      putRow i b
      pb (i + 1) bs

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  if isDigit x
    then do
      newline
      return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit 🤯"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  newline
  if finished board
    then do
      putStrLn ("Player " ++ show (next player) ++ " wins!!")
    else do
      putStrLn ("Turn of player " ++ show player ++ " 🐥")
      row <- getDigit " > Enter a row number: "
      num <- getDigit " > Stars to remove: "
      if valid board row num
        then play (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: Invalid move 🤯"
          play board player

nim :: IO ()
nim = play initial 1