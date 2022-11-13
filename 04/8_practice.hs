{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}

-- 1
halve :: [a] -> ([a], [a])
halve ns = (take hl ns, drop hl ns)
  where
    hl = length ns `div` 2

-- 2
-- third :: [a] -> a
-- third ns = head (tail (tail ns))
-- third ns = ns !! 3
-- third (_ : _ : a : _) = a

-- 3
safetail :: [a] -> [a]
-- safetail ns = if null ns then [] else tail ns
-- safetail ns
--   | null ns = []
--   | otherwise = tail ns
safetail [] = []
safetail (_ : xs) = xs

-- 4
(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

-- 7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- 8
luhnDouble :: Int -> Int
luhnDouble n
  | 9 < n * 2 = n * 2 - 9
  | otherwise = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum (map luhnDouble [a, b, c, d]) `mod` 10 == 0