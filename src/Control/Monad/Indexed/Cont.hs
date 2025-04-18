{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Indexed.Cont where

import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Weave
import Control.Monad.Indexed qualified as Indexed

newtype ContW w r r' a = ContW {runContW :: w (a -> r') -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Applicative (ContW w r r)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Monad (ContW w r r)

instance (Comonad w) => Indexed.Applicative (ContW w) where
  pure x = ContW $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (ContW w) where
  ContW a >>= f = ContW $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runContW (f x) wk

shift :: (Comonad w) => (w (a -> r') -> ContW w r k k) -> ContW w r r' a
shift f = ContW $ \wk -> runContW (f wk) (const id <$> wk)

handle :: (Comonad w) => ContW (StoreT k w) r r' a -> ContW w k r' a -> ContW w r r' a
handle (ContW inner) (ContW handler) =
  ContW $ \wk -> inner (StoreT (const <$> wk) (handler wk))

run :: (Comonad w) => (w (a -> r) -> r) -> ContW w r r a
run act = shift $ Indexed.pure . act

run' :: (Comonad w) => (w r -> r) -> ContW w r r ()
run' act = shift $ \wk -> Indexed.pure $ act (($ ()) <$> wk)

----------------------------------------------------------------------------
--
-- Exploratory: Stacked without explicit Cont2
--
----------------------------------------------------------------------------

empty :: (Weave w r) => ContW w r r' a
empty = ContW $ exit

(<|>) :: (Weave w r) => ContW w r r' a -> ContW w r r' a -> ContW w r r' a
(ContW a) <|> (ContW b) = ContW $ \wk -> (a `weave` b) wk
