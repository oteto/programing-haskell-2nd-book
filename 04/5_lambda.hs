-- p.43 - p.45
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- ラムダ式は無名関数
-- \x -> x + x
-- (\x -> x + x) 2   -- 4
-- add :: Int -> Int -> Int
-- add x y = x + y

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- odds :: Int -> [Int]
-- odds n = map f [0 .. n - 1]
--   where
--     f x = x * 2 - 1
-- ↓↓↓↓
odds :: Int -> [Int]
odds n = map (\x -> x * 2 - 1) [0 .. n - 1]