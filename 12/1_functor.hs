-- p.153 - p.157
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}

inc :: [Int] -> [Int]
inc [] = []
inc (n : ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n : ns) = n ^ 2 : sqr ns

-- 上記２つの関数を抽象化すると map 関数となる
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

{-
  map を使うと...

  inc = map (+1)
  sqr = map (^2)
-}

{-
  class Functor f where
    fmap :: (a -> b) -> f a -> f b

  instance Fanctor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map -- リスト型は単に fmap に map 代入しているだけ

  上のインスタンス宣言で [a] は [] a のもう一つの書き方なので f a -> f b に合っている ([] a -> [] b)
-}

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

{-
  fmap を使うと...

  inc = fmap (+1)
  sqr = fmap (^2)

  inc (Just' 1)
  > Just' 2

  inc [1,2,3]
  > [2,3,4]

  どの Functor（関手）にも適用できる
-}
