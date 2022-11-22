-- p.197 - p.201

import Data.Monoid (Monoid)

{-
  class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
-}

{-
  instance Monoid [a] where
    -- mempty :: [a]
    mempty = []

    -- mappend :: [a] -> [a] -> [a]
    mappend = (++)
-}

{-
  instance Monoid a => Monoid (Maybe a) where
    -- mempty :: Maybe a
    mempty = Nothing

    -- mappend :: Maybe a -> Maybe a -> Maybe a
    Nothing `mappend` my = my
    mx `mappend` mNothing = mx
    Just x `mappend` Just y = Just (x `mappend` y)
-}