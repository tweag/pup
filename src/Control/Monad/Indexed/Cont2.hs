{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Indexed.Cont2 where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Indexed qualified as Indexed

newtype Cont2W w r r' a = Cont2W {runCont2W :: w (a -> r' -> r') -> r -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Applicative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Monad (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Alternative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => MonadPlus (Cont2W w r r)

instance (Comonad w) => Indexed.Applicative (Cont2W w) where
  pure x = Cont2W $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (Cont2W w) where
  Cont2W a >>= f = Cont2W $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runCont2W (f x) wk

shift :: (Comonad w) => (w (a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
shift f = Cont2W $ \wk fl -> runCont2W (f wk fl) ((\_k -> \x _ -> x) <$> wk) fl

run :: (Comonad w) => (w (a -> r) -> r) -> Cont2W w r r a
run act = shift $ \wk fl -> Indexed.pure $ act (fmap (\k x -> k x fl) wk)

run' :: (Comonad w) => (forall s. w s -> s) -> Cont2W w r r ()
run' act = shift $ \wk fl -> Indexed.pure $ act (fmap (\k -> k () fl) wk)

instance (Comonad w) => Indexed.Stacked (Cont2W w) where
  empty = Cont2W $ \_ fl -> fl
  (Cont2W a) <|> (Cont2W b) = Cont2W $ \wk fl -> a wk (b wk fl)

  shift' f = shift (\wk -> f (extract (($ ()) <$> wk)))
