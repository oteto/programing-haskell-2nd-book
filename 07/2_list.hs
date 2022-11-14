-- p.76 - p.78

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- 再帰を使った定義
-- map' _ [] = []
-- map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' f xs = [x | x <- xs, f x]
-- 再帰を使った定義
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs