{-# LANGUAGE GADTSyntax #-}

module LazyMList where

data List m a where
  Nil  :: List m a
  Cons :: a -> m (List m a) -> List m a

type MList m a = m (List m a)

nil :: Monad m => MList m a
nil = return Nil

cons :: Monad m => a -> MList m a -> MList m a
cons a = return . Cons a

mfoldr :: Monad m => (a -> m r -> m r) -> m r -> MList m a -> m r
mfoldr f z ml = do
  l <- ml
  case l of
    Nil        -> z
    Cons a ml' -> f a (mfoldr f z ml')

mmap :: Monad m => (a -> b) -> MList m a -> MList m b
mmap f = mfoldr (cons . f) nil

example :: MList IO Int
example = print "a" >> cons 1 (print "b" >> (cons 2 (print "c" >> nil)))

mhead :: Monad m => MList m a -> m a
mhead ml = do
  l <- ml
  case l of
    Nil -> error "head Nil"
    Cons a _ -> return a

