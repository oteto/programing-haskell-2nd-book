-- p.81 - p.83

-- 左結合の演算子を使った sum の定義
sum' :: Num a => [a] -> a
sum' = sum'' 0
  where
    sum'' v [] = v
    sum'' v (x : xs) = sum'' (v + x) xs

-- foldl の定義
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x : xs) = foldl' f (f v x) xs