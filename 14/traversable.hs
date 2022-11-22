-- p.207 - p.210

traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' _ [] = pure []
traverse' g (x : xs) = (:) <$> g x <*> traverse' g xs

{-
  class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-}