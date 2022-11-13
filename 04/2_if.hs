-- p.39-40
-- If 式には必ず else が必要
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}

signum :: Int -> Int
signum n =
  if n < 0
    then -1
    else
      if n == 0
        then 0
        else 1