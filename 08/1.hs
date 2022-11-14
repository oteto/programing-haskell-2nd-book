-- p.93 - p.94

-- type による型宣言 先頭大文字
type String = [Char]

-- 宣言した型は他の宣言でも使用できる
type Pos = (Int, Int)

type Trans = Pos -> Pos

-- type による型宣言では再帰できない
-- type Tree = (Int, [Tree]) -- できない

-- 多相型
type Pair a = (a, a)

type Assoc k v = [(k, v)]