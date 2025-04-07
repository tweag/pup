{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Base.Print where

import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

newtype Print rf r r' a = Print {runPrint :: rf -> (a -> String -> rf -> r') -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (Print rf) r r) instance Prelude.Applicative (Print rf r r)

deriving via (Indexed.FromIndexed (Print rf) r r) instance Prelude.Monad (Print rf r r)

instance Indexed.Applicative (Print rf) where
  pure x = Print $ \fl k -> k x "" fl

instance Indexed.Monad (Print rf) where
  (Print a) >>= kp = Print $ \fl k -> a fl $ \x sx flx ->
    runPrint (kp x) flx $ \y sy fly -> k y (sx ++ sy) fly

instance Indexed.Stacked Print where
  empty = Print $ \fl _k -> fl
  (Print lft) <|> (Print rgt) = Print $ \fl k ->
    lft (rgt fl k) k

  stack f = Print $ \fl k -> f fl (k () mempty fl)
  handle h (Print prnt) =
    -- Note: the success continuation of `prnt` ignores its failure
    -- continuation. That's because we don't backtrack inside a `handle`.
    Print $ \fl k -> prnt (h fl (\x -> k x "" fl)) (\x sx _ -> k x sx fl)
