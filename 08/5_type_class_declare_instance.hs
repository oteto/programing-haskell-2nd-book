-- p.100 - p.102
{-# OPTIONS_GHC -Wno-missing-methods #-}

--  class を用いて型クラスを宣言できる

class Eq' a where
  (==.), (/=.) :: a -> a -> Bool
  x /=. y = not (x ==. y) -- デフォルト実装

-- instance を用いて、 Bool型を Eq' のインスタンスにする
instance Eq' Bool where
  False ==. False = True
  True ==. True = True
  _ ==. _ = False

-- クラスのインスタンスになれるのは data, newtype で宣言された型のみ

-- ある型クラスを拡張して新しい型クラスを作成することも可能
class Eq a => Ord' a where
  (<.), (<=.), (>.), (>=.) :: a -> a -> Bool
  min, max :: a -> a -> a
  min x y
    | x <=. y = x
    | otherwise = y
  max x y
    | x <=. y = y
    | otherwise = x

instance Ord' Bool where
  False <. True = True
  _ <. _ = False
  b <=. c = (b <. c) || (b == c)
  b >. c = c <. b
  b >=. c = c <=. b

{-
  deriving を使用して自動的に Eq, Ord, Show, Read などのインスタンスにできる

  data Bool = False | True deriving (Eq, Ord, Show, Read)
-}
