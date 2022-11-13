-- p.58 - p.60

-- 1
one :: Int
one = sum [x ^ 2 | x <- [1 .. 100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0 .. n], y <- [0 .. m]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
repli :: Int -> a -> [a]
repli n x = [x | _ <- [1 .. n]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- 6
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (p x) == x]
  where
    p n = [x | x <- [1 .. n - 1], n `mod` x == 0]

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
