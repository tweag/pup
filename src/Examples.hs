{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Combinators
import Control.Monad.Indexed ((<*), (<*>), (<|>))
import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..))

data T = C Int Bool | D Char Bool Int
  deriving (Show)

data U = K T Int | L
  deriving (Show)

uupT :: PUP (T -> r) r T
uupT =
  cLead <* string "C" <* space <*> int <* space <*> bool
    <|> dLead <* string "D" <* space <*> anyChar <* space <*> bool <* space <*> int
  where
    cLead = Indexed.do
      Indexed.stack (\cases _ k (C i b) -> k i b; fl _ t -> fl t) (\k i b -> k (C i b))
      Indexed.pure C
    dLead = Indexed.do
      Indexed.stack (\cases _ k (D c b i) -> k c b i; fl _ t -> fl t) (\k c b i -> k (D c b i))
      Indexed.pure D

uupU :: PUP (U -> r) r U
uupU =
  kLead <* string "K" <* space <*> uupT <* space <*> int
    <|> lLead <* string "L"
  where
    kLead = Indexed.do
      Indexed.stack (\cases _ k (K t i) -> k t i; fl _ t -> fl t) (\k t i -> k (K t i))
      Indexed.pure K
    lLead = Indexed.do
      Indexed.stack (\cases _ k L -> k; fl _ t -> fl t) (\k -> k L)
      Indexed.pure L
