-- p.53 - p.54

{-
  "abc" :: String = ['a', 'b', 'c'] :: [Char]

  > "abcde" !! 2
  'c'

  > take 3 "abcde"
  "abc"

  > length "abcde"
  5

  > zip "abcde" [1,2,3,4]
  [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
-}

lowers :: String -> Int
lowers xs = length [1 | x <- xs, 'a' <= x && x <= 'z']

count :: Char -> String -> Int
count x xs = length [1 | x' <- xs, x == x']