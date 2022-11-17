import Data.Char (isDigit)
import Data.List (transpose)

-- p.139 - p.150

size :: Int
size = 3

-- | まる、空、ばつ
data Player = O | B | X deriving (Eq, Ord, Show)

-- 0 1 2
-- 3 4 5
-- 6 7 8
type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

-- | 空の盤面を作成する（初期状態）
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

-- | 次のプレイヤーを返す
turn :: Grid -> Player
turn g = if os ps <= xs ps then O else X
  where
    ps = concat g
    os = length . filter (/= X)
    xs = length . filter (/= O)

-- | 斜め（左上 -> 右下）のプレイヤーを返す
-- （右上 -> 左下）は盤面を反転してから適用すれば取得できる
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

-- | 指定したプレイヤーが勝利したかどうかを返す
wins :: Player -> Grid -> Bool
wins p g = any line (g ++ cols ++ dias)
  where
    line = all (== p)
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- | どちらかのプレイヤーが勝利したかどうかを返す
won :: Grid -> Bool
won g = wins O g || wins X g

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer B = ["   ", "   ", "   "]

-- | リストの各要素間に値を差し込む
interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

-- | マスをとることができるかを返す
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- | 指定のマスをプレイヤーのマスにする
move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop size (xs ++ [p] ++ ys) | valid g i]
  where
    (xs, B : ys) = splitAt i (concat g)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number 🤯"
      getNat prompt

-- | 画面のクリア
cls :: IO ()
cls = putStr "\ESC[2J"

-- | 指定の位置にカーソルを持っていく
goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "🎉 Player O wins!\n"
  | wins X g = putStrLn "🎉 Player X wins!\n"
  | full g = putStrLn "🙏 It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move 🤯"
        run' g p
      [g'] -> run g' (next p)
      _ -> putStrLn "ERROR: Exception game 🔥"

tictactoe :: IO ()
tictactoe = run empty O
