-- p.139 - p.150
import Data.Char (isDigit)
import Data.List (transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

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

data Tree a = Node a [Tree a] deriving (Show)

-- | 次に遷移できるマスの構造のリストを返す
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

-- | とりうる全てのマス配置木構造を返す
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- | 与えられた深さ以降の Node を削除する
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- | 木構造の最大の深さ
depth :: Int
depth = 9

-- | 盤面の木構造の各 Node にプレイヤーのラベルを付与する
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) -- 葉の場合はその時点の勝者をラベルに設定する、いない場合は空をラベルに設定する
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts) -- 節の場合は１階層下の子のラベルに対して、プレイヤーが O ならの最小値を、X の場合は最大値をラベルに設定する
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node ((_, p)) _ <- ts']

-- | 与えられた木構造の根と同じラベルの Node が最善手となる
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree -- ts は１階層したの Node のリスト

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "🎉 Player O wins!\n"
  | wins X g = putStrLn "🎉 Player X wins!\n"
  | full g = putStrLn "🙏 It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move 🤯"
          play' g p
        [g'] -> play g' (next p)
        _ -> putStrLn "ERROR: Exception game 🔥"
  | p == X = do
      putStr "Player X is thinking..."
      (play $! (bestmove g p)) (next p)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O
