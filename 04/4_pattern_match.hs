-- p.41 - 43
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

not :: Bool -> Bool
not False = True
not True = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False
-- ↓ 下の３つは以下のように書ける
-- _ && _ = False
-- 実際のプレリュードでは...
(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False -- 遅延評価により、２つ目の _ は評価されない

-- 4.4.1 タプルパターン
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

-- 4.4.2 リストパターン

-- リストの長さが 3 で、先頭の文字が a であれば True を返す関数
test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

-- [1,2,3] は 1 : (2 : (3 : [])) の略記法
-- `:` <- cons 演算子 (右結合) (リストの先頭に新しい要素を増やす演算子)
-- ↓↓↓↓
-- [1,2,3]
-- = 1 : [2,3]
-- = 1 : (2 : [3])
-- = 1 : (2 : (3 : []))

-- cons 演算子を使って test 関数は以下のように書き換えられる
-- test :: [Char] -> Bool
-- test ('a':_) = True -- 関数適用は演算子より結合順位が高いため () が必要
-- test _ = False

-- プレリュードの head, tail
head :: [a] -> a
head (x : _) = x

tail :: [a] -> [a]
tail (_ : xs) = xs