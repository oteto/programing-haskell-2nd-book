-- p.91 - p.92
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Use uncurry" #-}

-- 1
-- [f x | x <- xs, p x]

mf :: (a -> Bool) -> (a -> b) -> [a] -> [b]
mf p f = map f . filter p

-- 2
-- a
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = xs

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int ns = sum [w * d | (w, d) <- zip ws (reverse ns)]
  where
    ws = iterate (* 10) 1

-- 5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs