-- p.75 - p.76
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- 整数 x をとって関数を返す
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- 関数を引数に指定することも可能
twice :: (a -> a) -> a -> a
twice f x = f (f x)
