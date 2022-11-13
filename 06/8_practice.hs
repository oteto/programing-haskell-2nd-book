-- p.73 - p.74
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}

-- 1

fac :: Int -> Int
fac 0 = 1
fac n | 0 < n = n * fac (n - 1)

-- 2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3
(#) :: Int -> Int -> Int
m # 0 = 1
m # n = m * (m # (n - 1))

-- 4
euclid :: Int -> Int -> Int
euclid n m
  | n == m = n
  | n < m = euclid n (m - n)
  | m < n = euclid m (n - m)

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 8

halve :: [a] -> ([a], [a])
halve ns = (take hl ns, drop hl ns)
  where
    hl = length ns `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort bs) (msort as)
  where
    (bs, as) = halve xs
