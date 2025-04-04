{-# LANGUAGE DerivingStrategies #-}

module Base.Print where

import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..))

newtype Print rf r r' a = Print {runPrint :: rf -> (a -> String -> rf -> r') -> r}
  deriving stock (Functor)

instance Indexed.Applicative (Print rf) where
  pure x = Print $ \fl k -> k x "" fl

instance Indexed.Monad (Print rf) where
  (Print a) >>= kp = Print $ \fl k -> a fl $ \x sx flx ->
    runPrint (kp x) flx $ \y sy fly -> k y (sx ++ sy) fly

instance Indexed.Stacked Print where
  empty = Print $ \fl _k -> fl
  (Print lft) <|> (Print rgt) = Print $ \fl k ->
    lft (rgt fl k) k
