-- p.78 - p.81
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sum" #-}

sum' :: Num a => [a] -> a
sum' = foldr (+) 0 -- sum' xs = foldr (+) 0 xs とも書ける

-- 再帰を使った定義
-- sum' [] = 0
-- sum' (x : xs) = x + sum' xs

-- foldr の定義
-- 右から畳み込まれていく
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

-- プレリュード関数 length を foldr で定義
length' :: [a] -> Int
length' = foldr' (\_ n -> n + 1) 0

-- プレリュード関数 reverse を foldr で定義
reverse' :: [a] -> [a]
reverse' = foldr' snoc []
  where
    snoc x xs = xs ++ [x] -- cons 演算子 `:` の逆