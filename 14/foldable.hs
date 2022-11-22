-- p.201 - p.207

{-
  class Foldable t where
    fold :: Monoid a => t a -> a
    foldMap :: Monoid a => (a -> b) -> t a -> b
    -- 以下二つは初期値と結合用の関数を明示的に渡すので、Monoid の制約はない
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (a -> b -> a) -> a -> t b -> a
-}

{-
  instance Foldable [] where
    -- fold :: Monoid a => [a] -> a
    fold [] = mempty
    fold (x:xs) = x `mappend` fold xs

    -- foldMap :: Monoid a => (a -> b) -> [a] -> b
    foldMap _ [] = mempty
    foldMap f (x:xs) = f x `mappend` foldMap f xs

    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ v [] = v
    foldr f v (x:xs) = f x (foldr f v xs)

    -- foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl _ v [] = v
    foldl f v (x:xs) = foldl f (f x v) xs
-}