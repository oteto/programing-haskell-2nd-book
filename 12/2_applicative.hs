-- p.157 - p.164

{-
  class Functor f => Applicative f where
    pure:: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  -- pure :: a -> Maybe' a
  pure = Just'

  -- (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  (Just' g) <*> mx = fmap g mx
