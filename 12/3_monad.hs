-- p. 164 - p.174

{-
  class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    return = pure

    instance Monad Maybe where
      -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
      Nothing >>= _ = Nothing
      (Just x) >>= f = f x

    instance Monad [] where
      -- (>>=) :: [a] -> (a -> [b]) -> [b]
      xs >>= f = [y | x <- xs, y <- f x]
-}

type State = Int

-- | 状態変換器
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

-- step1 Functor
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

-- step2 Applicative
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

-- step3 Monad
instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- 木にラベル付けする関数

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- | applicative
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- | monad
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')