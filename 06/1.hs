-- p.61 - 62

-- fac :: Int -> Int
-- fac n = product [1 .. n]
-- ↓↓↓↓ 再帰を用いて
fac :: Int -> Int
fac 0 = 1 -- 基底部
fac n = n * fac (n - 1) -- 再帰部

-- * を再帰で + の連続であることを定式化する
(#) :: Int -> Int -> Int
m # 0 = 0
m # n = m + (m # (n - 1))