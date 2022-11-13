-- p.40
-- guard

abs n
  | 0 <= n = n
  | otherwise = -n

-- if より簡潔に書ける
signum n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1