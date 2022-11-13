-- p.52 - p.53

{-
  プレリュード関数 zip
  > zip ['a', 'b', 'c'] [1, 2, 3, 4]
  [('a', 1), ('b', 2), ('c', 3)]
-}

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x'] -- [0..] は無限の要素を持つが、遅延評価により必要な分のみ生成される
