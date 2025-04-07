{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Monad.Indexed where

import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import GHC.Stack (HasCallStack)
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

class (forall i j. Functor (f i j)) => Applicative f where
  pure :: a -> f i i a

  (<*>) :: f i j (a -> b) -> f j k a -> f i k b
  default (<*>) :: (Monad f) => f i j (a -> b) -> f j k a -> f i k b
  ff <*> aa = ff >>= \f -> aa >>= \a -> pure (f a)

  liftA2 :: (a -> b -> c) -> f i j a -> f j k b -> f i k c
  liftA2 f x = (<*>) (fmap f x)

  (*>) :: f i j a -> f j k b -> f i k b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f i j a -> f j k b -> f i k a
  (<*) = liftA2 const

class (Applicative m) => Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b

-- No equivalent to Alternative just because we don't need it. But it's, of
-- course, not a problem.
class (forall x. Monad (m x)) => Stacked m where
  empty :: m i i j a
  (<|>) :: m i i j a -> m i i j a -> m i i j a

  stack :: (x -> j -> i) -> m x i j ()

  -- | The continuationy style make it a little hard to read what's going
  -- on. The general idea is that:
  --
  -- * if you want to re-raise, you call the `y` type continuation.
  --
  -- * if you want to return a value, you call the `a -> j` type continuation
  --
  -- * a payload in failure `x` is passed in the form `pld -> x'`
  --
  -- Depending on the types you may not be able to re-raise or return a value.
  --
  -- /e.g./
  -- > type T = A Int | B | C
  -- > p :: M (T -> y) (Maybe Int) j (Maybe Int)
  -- >
  -- > q :: M y (Maybe Int) j (Maybe Int)
  -- > q = handle (\fl k t ->
  -- >     case t of
  -- >       A n -> Just n
  -- >       B -> Nothing
  -- >       C -> fl
  -- >   ) p
  handle :: (y -> (a -> j) -> x) -> m x i j a -> m y i j a

(@) :: (Stacked m) => m x (a -> i) j b -> a -> m x i j b
act @ a = stack (\_ s -> s a) *> act

infixl 9 @

-- Asserts that an `m x i j a` computation is complete. In that it can never
-- raise an error.
complete :: (HasCallStack, Stacked m) => m x i j a -> m y i j a
complete = handle (\_ _ -> error "This printer wasn't complete")

newtype IgnoreStack m x i j a = IgnoreStack {unIgnoreStack :: m a}
  deriving newtype (Functor)

instance (Prelude.Applicative f) => Applicative (IgnoreStack f x) where
  pure a = IgnoreStack $ Prelude.pure a
  IgnoreStack f <*> IgnoreStack a = IgnoreStack $ f Prelude.<*> a

instance (Prelude.Monad m) => Monad (IgnoreStack m x) where
  IgnoreStack a >>= k = IgnoreStack $ a Prelude.>>= \x -> unIgnoreStack (k x)

instance (Monad.MonadPlus m) => Stacked (IgnoreStack m) where
  empty = IgnoreStack Applicative.empty
  (IgnoreStack a) <|> (IgnoreStack b) = IgnoreStack $ a Applicative.<|> b

  stack _ = IgnoreStack $ Prelude.pure ()
  handle _ (IgnoreStack a) = IgnoreStack a

data (:*:) f g x i j a = (:*:) (f x i j a) (g x i j a)

instance (Functor (f x i j), Functor (g x i j)) => Functor ((f :*: g) x i j) where
  fmap f (a :*: b) = (fmap f a) :*: (fmap f b)

instance (Applicative (f x), Applicative (g x)) => Applicative ((f :*: g) x) where
  pure a = (pure a) :*: (pure a)

  (f :*: f') <*> (a :*: a') = (f <*> a) :*: (f' <*> a')

instance (Monad (f x), Monad (g x)) => Monad ((f :*: g) x) where
  (a :*: b) >>= k =
    (a >>= \x -> let (r :*: _) = k x in r)
      :*: (b >>= \y -> let (_ :*: s) = k y in s)

instance (Stacked f, Stacked g) => Stacked (f :*: g) where
  empty = empty :*: empty

  (a :*: a') <|> (b :*: b') = (a <|> b) :*: (a' <|> b')

  stack f = (stack f) :*: (stack f)

  handle h (a :*: b) = (handle h a :*: handle h b)
