{-# LANGUAGE DerivingVia #-}

module Base.Print where

import Control.Applicative
import Control.Monad
import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

newtype Print r r' a = Print {runPrint :: r -> (a -> String -> r' -> r') -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed Print r r) instance Prelude.Applicative (Print r r)

deriving via (Indexed.FromIndexed Print r r) instance Prelude.Monad (Print r r)

deriving via (Indexed.FromIndexed Print r r) instance Alternative (Print r r)

deriving via (Indexed.FromIndexed Print r r) instance MonadPlus (Print r r)

instance Indexed.Applicative Print where
  pure x = Print $ \fl k -> k x "" fl

instance Indexed.Monad Print where
  (Print a) >>= kp = Print $ \fl k -> a fl $ \x sx flx ->
    runPrint (kp x) flx $ \y sy fly -> k y (sx ++ sy) fly

instance Indexed.Stacked Print where
  empty = Print $ \fl _k -> fl
  (Print lft) <|> (Print rgt) = Print $ \fl k ->
    lft (rgt fl k) k

  stack f unr = Print $ \fl k -> f fl (k () mempty (unr fl))

print :: forall r a. (Indexed.Unroll r (Maybe String)) => Print r (Maybe String) a -> r
print prnt = runPrint prnt (Indexed.unroll @r @(Maybe String) Nothing) (\_ s _ -> Just s)

anyChar :: Print (Char -> r) r Char
anyChar = Print $ \fl k c -> k c [c] (fl c)

once :: (r -> r') -> Print r r' a -> Print r r' a
once unr (Print a) = Print $ \fl k -> a fl $ \x sx _flx -> k x sx (unr fl)
