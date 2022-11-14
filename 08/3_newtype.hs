-- p.96

-- 構成子が１つ引数も１つの場合、newtype で宣言できる
newtype Nat = N Int

-- type Nat = Int -- 同じではない別の型

-- data Nat = N Int -- 大体同じ。 newtype では構成子が型検査後にコンパイラによって削除されるのでコスト的に良い