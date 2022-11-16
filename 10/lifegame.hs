-- p.132 - p.136

-- | 座標 (x, y)
type Pos = (Int, Int)

type Board = [Pos]

-- | 画面のクリア
cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

-- | 指定の位置にカーソルを持っていく
goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells b = sequence_ [writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isDead :: Board -> Pos -> Bool
isDead b = not . isAlive b

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- | 周りの座標を返す
neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

-- | 生存している周りのマスの数を返す
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- | 次も生き残るマスを返す
survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

-- | 次に生まれるマスを返す
births :: Board -> [Pos]
-- births b = [(x, y) | x <- [1 .. width], y <- [1 .. height], isDead b (x, y), liveneighbs b (x, y) == 3]
births b = [p | p <- rmdups (concatMap neighbs b), isDead b p, liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

width :: Int
width = 10

height :: Int
height = 10

-- | 初期生存セル
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 500000
  life (nextgen b)
