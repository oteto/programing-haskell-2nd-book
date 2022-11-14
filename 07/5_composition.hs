-- p.83 - p.84

{-
  プレリュードで定義される (.) は２つの関数を合成下関数を返す高階演算子
  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  f . g = \x -> f (g x)

  関数合成を使用すると、括弧の数が減り、引数を省略できる

  odd n = not (even n)
  odd = not . even

  twice f x = f (f x)
  twice f = f . f

  -- 適切な型を持つ任意の関数 f, g, h に対し、(f . g) . h = f . (g . h) が成り立つ（結合則）
  sumsqreven ns = sum (map (^2) (filter even ns))
  sumsqreven = sum . map (^2) . filter even
-}