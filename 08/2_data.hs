-- p.94 - p.96

-- data を使って既存の方の別名ではない新しい方を宣言できる
data Bool = False | True

-- 型の値（以下だと North など）のことを構成子と呼ぶ。構成子も先頭大文字でなければいけない
-- また、同じ名前の構成子を複数の型で使いことはできない
data Move = North | South | East | West

-- data Move2 = North | South -- 同じ構成子は使えない
type Pos = (Int, Int)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (flip move) p ms

rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West

-- 構成子に引数を渡せる
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- 型変数も使える
data Maybe a = Nothing | Just a