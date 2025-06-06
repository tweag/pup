{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module SelfContained.Shared where

import Control.Monad (ap)
import Prelude hiding (Applicative (..), Monad (..), (<$>))
import Prelude qualified

class IxMonad m where
  return :: a -> m r r a
  (>>=) :: m r' r a -> (a -> m r'' r' b) -> m r'' r b

(<$>) :: (IxMonad m) => (a -> b) -> m r r' a -> m r r' b
f <$> ma = ma >>= \a -> return (f a)

(<*>) :: (IxMonad m) => m r' r (a -> b) -> m r'' r' a -> m r'' r b
mf <*> ma = mf >>= \f -> ma >>= \a -> return (f a)

(*>) :: (IxMonad m) => m r' r () -> m r'' r' a -> m r'' r a

ma <* mu = ((\a _ -> a) <$> ma) <*> mu

(<*) :: (IxMonad m) => m r' r a -> m r'' r' () -> m r'' r a

mu *> ma = ((\_ a -> a) <$> mu) <*> ma

class (Functor w) => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b

newtype Traced m a = Traced {runTraced :: m -> a}
  deriving (Functor)

instance (Monoid m) => Comonad (Traced m) where
  extract (Traced a) = a mempty
  extend f (Traced a) = Traced \m ->
    f (Traced \m' -> a (m <> m'))

class (Comonad w) => ComTraced m w where
  trace :: m -> w a -> a

instance (Monoid m) => ComTraced m (Traced m) where
  trace x (Traced f) = f x

newtype Prs a
  = Prs {runPrs :: String -> Maybe (a, String)}
  deriving (Functor)

instance Prelude.Monad Prs where
  -- return a = Prs \s -> Just (a, s)
  (Prs p) >>= f = Prs \s -> do
    ~(a, s') <- p s
    runPrs (f a) s'

instance Prelude.Applicative Prs where
  pure a = Prs \s -> Just (a, s)
  (<*>) = ap

data (f :*: g) r r' a
  = (:*:) {ifst :: (f r r' a), isnd :: (g r r' a)}

instance
  (IxMonad f, IxMonad g) =>
  IxMonad (f :*: g)
  where
  return x = return x :*: return x
  ~(l :*: r) >>= f =
    (l >>= (ifst . f)) :*: (r >>= (isnd . f))

newtype Fwd m r r' a = Fwd {unFwd :: m a}

instance (Prelude.Monad m) => IxMonad (Fwd m) where
  return x = Fwd (Prelude.return x)
  (Fwd a) >>= f = Fwd (a Prelude.>>= (unFwd . f))

