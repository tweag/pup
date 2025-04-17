{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Combinators
import Control.Monad
import Control.Monad.Indexed ((<*), (<*>), (<|>))
import Control.Monad.Indexed qualified as Indexed
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

-- An example of backtracking behaviour
bktrk :: PUP r r ()
bktrk = Indexed.do
  b <- Indexed.pure True <|> Indexed.pure False
  guard $ not b
  case b of
    True -> string "True"
    False -> string "False"

-- Why does (<|>) backtrack in Print. Because:
--
-- pure True <|> pure False = Print $ \fl k -> k True "" (k False "" fl)
--
-- so
--
-- (pure True <|> pure False) >>= f = Print $ \fl k ->
--   let k x sk flx = runPrint (f x) flx $ \y sy fly -> k y (sx ++ sy) fly in
--   k True "" (k False "" fl)

-- Avoiding backtracking
bktrk_once :: Bool -> PUP r r ()
bktrk_once b0 = Indexed.do
  b <- Combinators.once id (Indexed.pure True <|> Indexed.pure False)
  guard $ b == b0
  case b of
    True -> string "True"
    False -> string "False"
