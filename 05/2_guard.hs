-- p.50 - p.52

-- > [x | x <- [1..10], even x]
--                     ~~~~~~~~ guard
-- [2, 4, 6, 8, 10]

-- 正の整数に対して約数を全て計算する関数
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
--                             ~~~~~~~~~~~~~~~~ guard

-- 素数判定
prime :: Int -> Bool
prime n = factors n == [1, n] -- 遅延評価により、２つ目の約数が評価された時点で値が決まる

-- 与えられた上限までの素数を生成
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]
--                            ~~~~~~~~~ guard

find :: Eq a => a -> [(a,b)] -> [b]
find k ts = [v | (k', v) <- ts, k == k']
--                              ~~~~~~~ guard