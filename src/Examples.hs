{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Combinators
import Control.Monad.Indexed ((<*), (<*>), (<|>))
import GHC.Generics
import Generic (lead)
import Prelude hiding (Applicative (..), Monad (..))

data T = C Int Bool | D Char Bool Int
  deriving (Show, Generic)

data U = K T Int | L
  deriving (Show, Generic)

uupT :: PUP (T -> r) r T
uupT =
  lead @"C" <* string "C" <* space <*> int <* space <*> bool
    <|> lead @"D" <* string "D" <* space <*> anyChar <* space <*> bool <* space <*> int

uupU :: PUP (U -> r) r U
uupU =
  lead @"K" <* string "K" <* space <*> uupT <* space <*> int
    <|> lead @"L" <* string "L"
