-- p.39
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

slpitAt :: Int -> [a] -> ([a], [a])
slpitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n