{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QualifiedDo #-}

module Control.Monad.Indexed where

import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import GHC.Stack (HasCallStack)
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

class (forall i j. Functor (f i j), forall i. Prelude.Applicative (f i i)) => Applicative f where
  pure :: a -> f i i a

  (<*>) :: f i j (a -> b) -> f j k a -> f i k b
  default (<*>) :: (Monad f) => f i j (a -> b) -> f j k a -> f i k b
  ff <*> aa = ff >>= \f -> aa >>= \a -> pure (f a)
  infixl 4 <*>

  liftA2 :: (a -> b -> c) -> f i j a -> f j k b -> f i k c
  liftA2 f x = (<*>) (fmap f x)

  (*>) :: f i j a -> f j k b -> f i k b
  a1 *> a2 = (id <$ a1) <*> a2
  infixl 4 *>

  (<*) :: f i j a -> f j k b -> f i k a
  (<*) = liftA2 const
  infixl 4 <*

class (Applicative m, forall i. Prelude.Monad (m i i)) => Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b

-- | For `QualifiedDo` notation
(>>) :: Applicative m => m i j () -> m j k a -> m i k a
(>>) = (*>)

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

guard :: Stacked m => Bool -> m i i i ()
guard p = if p then pure () else empty

-- some :: Stacked m => (forall r'. m r' (a -> r') r' b) -> m ([a] -> r) ([a] -> r) r [b]
-- some a = Control.Monad.Indexed.do
--   stack uncons
--   (:) <$> a <*> complete (many a)
--   where
--     uncons fl _k [] = fl []
--     uncons _fl k (x:xs) = k x xs

-- many :: Stacked m => (forall r'. m r' (a -> r') r' b) -> m x ([a] -> r) r [b]
-- many a = complete $ some a <|> pure []

newtype IgnoreStack m x i j a = IgnoreStack {unIgnoreStack :: m a}
  deriving newtype (Functor, Prelude.Applicative, Prelude.Monad)

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

instance (Prelude.Applicative (f x i j), Prelude.Applicative (g x i j)) => Prelude.Applicative ((f :*: g) x i j) where
  pure a = (Prelude.pure a) :*: (Prelude.pure a)
  (f :*: f') <*> (a :*: a') = (f Prelude.<*> a) :*: (f' Prelude.<*> a')

instance (Applicative (f x), Applicative (g x)) => Applicative ((f :*: g) x) where
  pure a = (pure a) :*: (pure a)

  (f :*: f') <*> (a :*: a') = (f <*> a) :*: (f' <*> a')

instance (Prelude.Monad (f x i j), Prelude.Monad (g x i j)) => Prelude.Monad ((f :*: g) x i j) where
  (a :*: b) >>= k =
    (a Prelude.>>= \x -> let (r :*: _) = k x in r)
      :*: (b Prelude.>>= \y -> let (_ :*: s) = k y in s)

instance (Monad (f x), Monad (g x)) => Monad ((f :*: g) x) where
  (a :*: b) >>= k =
    (a >>= \x -> let (r :*: _) = k x in r)
      :*: (b >>= \y -> let (_ :*: s) = k y in s)

instance (Stacked f, Stacked g) => Stacked (f :*: g) where
  empty = empty :*: empty

  (a :*: a') <|> (b :*: b') = (a <|> b) :*: (a' <|> b')

  stack f = (stack f) :*: (stack f)

  handle h (a :*: b) = (handle h a :*: handle h b)


-- | A deriving via combinator
newtype FromIndexed m i j a = FromIndexed (m i j a)
  deriving (Functor)


instance (Applicative m, i~j) => Prelude.Applicative (FromIndexed m i j) where
  pure x = FromIndexed $ pure x
  (FromIndexed f) <*> (FromIndexed a)= FromIndexed $ f <*> a

instance (Monad m, i~j) => Prelude.Monad (FromIndexed m i j) where
  (FromIndexed a) >>= k = FromIndexed $ a >>= \x ->
    let (FromIndexed b) = k x in b
