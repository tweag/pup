{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Monad.Indexed where

import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
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
(>>) :: (Applicative m) => m i j () -> m j k a -> m i k a
(>>) = (*>)

-- | Like for indexed monads and applicatives, we only have `MonadPlus` for
-- `m i i`. So we need custom combinators for empty and <|>.
class (Monad m, forall i. Monad.MonadPlus (m i i)) => Stacked m where
  empty :: m i j a
  (<|>) :: m i j a -> m i j a -> m i j a
  infixl 3 <|>

  -- Alternative: we could have an Applicative modality `S m`, and a richer shift
  -- shiftm :: ((S m a -> r' -> r') -> r -> m r r'' r'') -> m r r' (S m a)
  --
  -- `S Print = Identity`, and `S Parse = Const ()`. The latter is why we need
  -- the modality (and why it'd need to be an attached type family). It's kind
  -- of cool. And you could define
  --
  -- pop :: m (a -> i) i (S m a)
  -- push :: S m a -> m i (a -> i)
  --
  -- But is uses a type family, which is always a little bit annoying I suppose.
  --
  -- I'm not sure it adds much expressive power, though. It wouldn't be terribly
  -- convenient to use a value behind such a modality. You might as well just
  -- use `shift'` honestly.
  shift_ :: ((r' -> r') -> r -> m r r'' r'') -> m r r' ()

stack :: (Stacked m) => (i -> j -> i) -> (i -> j) -> m i j ()
stack f unr = shift_ $ \k fl -> pure $ f fl (k (unr fl))

(@) :: (Stacked m) => m (a -> i) j b -> a -> m i j b
act @ a = stack (\_ s -> s a) (\s _ -> s) *> act

infixl 9 @

pop' :: (Stacked m) => m (a -> i) i ()
pop' = shift_ $ \k fl -> pure (\a -> k (fl a))

some :: (Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
some a = Control.Monad.Indexed.do
  stack uncons (\k x xs -> k (x : xs))
  (:) <$> a <*> many a
  where
    uncons fl _k [] = fl []
    uncons _fl k (x : xs) = k x xs

many :: (Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
many a = some a <|> (pop' *> pure [])

newtype IgnoreStack m i j a = IgnoreStack {unIgnoreStack :: m a}
  deriving newtype
    ( Functor,
      Prelude.Applicative,
      Prelude.Monad,
      Applicative.Alternative,
      Monad.MonadPlus
    )

instance (Prelude.Applicative f) => Applicative (IgnoreStack f) where
  pure a = IgnoreStack $ Prelude.pure a
  IgnoreStack f <*> IgnoreStack a = IgnoreStack $ f Prelude.<*> a

instance (Prelude.Monad m) => Monad (IgnoreStack m) where
  IgnoreStack a >>= k = IgnoreStack $ a Prelude.>>= \x -> unIgnoreStack (k x)

instance (Monad.MonadPlus m) => Stacked (IgnoreStack m) where
  empty = IgnoreStack Applicative.empty
  (IgnoreStack a) <|> (IgnoreStack b) = IgnoreStack $ a Applicative.<|> b
  shift_ _ = IgnoreStack $ Prelude.pure ()

data (:*:) f g i j a = (:*:) (f i j a) (g i j a)
  deriving stock (Functor)

-- ⚠️ All the patterns on `x :*: y` must be lazy lest definitions like `many` start
-- looping because they become strict in their recursive arguments.
fst_star :: (f :*: g) i j a -> f i j a
fst_star ~(x :*: _) = x

snd_star :: (f :*: g) i j a -> g i j a
snd_star ~(_ :*: y) = y

instance (Prelude.Applicative (f i j), Prelude.Applicative (g i j)) => Prelude.Applicative ((f :*: g) i j) where
  pure a = (Prelude.pure a) :*: (Prelude.pure a)
  ~(f :*: f') <*> (a :*: a') = (f Prelude.<*> a) :*: (f' Prelude.<*> a')

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a = (pure a) :*: (pure a)

  ~(f :*: f') <*> ~(a :*: a') = (f <*> a) :*: (f' <*> a')
  ~(a :*: a') *> ~(b :*: b') = (a *> b) :*: (a' *> b')
  ~(a :*: a') <* ~(b :*: b') = (a <* b) :*: (a' <* b')

instance (Prelude.Monad (f i j), Prelude.Monad (g i j)) => Prelude.Monad ((f :*: g) i j) where
  ~(a :*: b) >>= k =
    (a Prelude.>>= \x -> let (r :*: _) = k x in r)
      :*: (b Prelude.>>= \y -> let (_ :*: s) = k y in s)

instance (Monad f, Monad g) => Monad (f :*: g) where
  ~(a :*: b) >>= k =
    (a >>= \x -> let (r :*: _) = k x in r)
      :*: (b >>= \y -> let (_ :*: s) = k y in s)

instance (Applicative.Alternative (f i j), Applicative.Alternative (g i j)) => Applicative.Alternative ((f :*: g) i j) where
  empty = Applicative.empty :*: Applicative.empty
  ~(a :*: a') <|> ~(b :*: b') = (a Applicative.<|> b) :*: (a' Applicative.<|> b')

instance (Monad.MonadPlus (f i j), Monad.MonadPlus (g i j)) => Monad.MonadPlus ((f :*: g) i j)

instance (Stacked f, Stacked g) => Stacked (f :*: g) where
  empty = empty :*: empty
  ~(a :*: a') <|> ~(b :*: b') = (a <|> b) :*: (a' <|> b')
  shift_ f = (shift_ (\s fl' -> fst_star (f s fl'))) :*: (shift_ (\s fl' -> snd_star (f s fl')))

-- | A deriving via combinator
newtype FromIndexed m i j a = FromIndexed (m i j a)
  deriving (Functor)

instance (Applicative m, i ~ j) => Prelude.Applicative (FromIndexed m i j) where
  pure x = FromIndexed $ pure x
  (FromIndexed f) <*> (FromIndexed a) = FromIndexed $ f <*> a

instance (Monad m, i ~ j) => Prelude.Monad (FromIndexed m i j) where
  (FromIndexed a) >>= k =
    FromIndexed $
      a >>= \x ->
        let (FromIndexed b) = k x in b

instance (Stacked m, i ~ j) => Applicative.Alternative (FromIndexed m i j) where
  empty = FromIndexed empty
  (FromIndexed a) <|> (FromIndexed b) = FromIndexed $ a <|> b

instance (Stacked m, i ~ j) => Monad.MonadPlus (FromIndexed m i j)

--------------------------------------------------------------

class Unroll i j where
  unroll :: j -> i

instance {-# OVERLAPPING #-} Unroll i i where
  unroll = id

instance (Unroll i j) => Unroll (a -> i) j where
  unroll j _a = unroll j
