{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Combinators
import Control.Monad
import Control.Monad.Indexed ((<*), (<*>), (<|>))
import Control.Monad.Indexed qualified as Indexed
import GHC.Generics
import Generic (lead)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main qualified as Hedgehog
import Hedgehog.Range qualified as Range
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

-------------------------------------------------------------------------------
--
-- Generic properties of pups
--
-------------------------------------------------------------------------------

roundTrip :: (Monad m, Eq a, Show a) => PUP (a -> Maybe String) (Maybe String) a -> a -> PropertyT m ()
roundTrip pp a = a' === Just (a, "")
  where
    a' = do
      str <- Combinators.print pp a
      Combinators.parse pp str

-------------------------------------------------------------------------------
--
-- Some basic data type pups
--
-------------------------------------------------------------------------------

data T = C Int Bool | D Char Bool Int
  deriving (Show, Generic, Eq)

genT :: (MonadGen m) => m T
genT = Gen.choice [genC, genD]
  where
    genC = C Prelude.<$> Gen.int (Range.linear 0 100) Prelude.<*> Gen.bool
    genD = D Prelude.<$> Gen.alphaNum Prelude.<*> Gen.bool Prelude.<*> Gen.int (Range.linear 0 100)

uupT :: PUP (T -> r) r T
uupT =
  lead @"C" <* string "C" <* space <*> int <* space <*> bool
    <|> lead @"D" <* string "D" <* space <*> anyChar <* space <*> bool <* space <*> int

data U = K T Int | L
  deriving (Show, Generic, Eq)

genU :: (MonadGen m) => m U
genU = Gen.frequency [(20, genK), (1, genL)]
  where
    genK = K Prelude.<$> genT Prelude.<*> Gen.int (Range.linear 0 100)
    genL = return L

uupU :: PUP (U -> r) r U
uupU =
  lead @"K" <* string "K" <* space <*> uupT <* space <*> int
    <|> lead @"L" <* string "L"

prop_round_trip_T :: Property
prop_round_trip_T = property $ do
  x <- forAll genT
  roundTrip uupT x

prop_round_trip_U :: Property
prop_round_trip_U = property $ do
  x <- forAll genU
  roundTrip uupU x

-------------------------------------------------------------------------------
--
-- Backtracking behaviour
--
-------------------------------------------------------------------------------

-- An example of backtracking behaviour
bktrk :: PUP r r ()
bktrk = Indexed.do
  b <- Indexed.pure True <|> Indexed.pure False
  guard $ not b
  case b of
    True -> string "True"
    False -> string "False"

prop_print_bktrk :: Property
prop_print_bktrk =
  property $
    Combinators.print bktrk === Just "False"

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

prop_print_bktrk_once :: Property
prop_print_bktrk_once = property $ do
  Combinators.print (bktrk_once True) === Just "True"
  Combinators.print (bktrk_once False) === Nothing

main :: IO ()
main = Hedgehog.defaultMain [tests]

tests :: IO Bool
tests = checkParallel $$(discover)
