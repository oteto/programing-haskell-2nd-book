-- p.63 - p.65
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

product' :: Num a => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[] +++ xs = xs
(x : xs) +++ ys = x : (xs +++ ys)

-- 整列されたリストに整列を保ちつつ要素を追加する関数
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- insert sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)