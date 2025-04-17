{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Combinators where

import Base.Parse (Parse (..))
import Base.Parse qualified as Parse
import Base.Print (Print)
import Base.Print qualified as Print
import Control.Monad
import Control.Monad.Indexed ((<*), (<|>))
import Control.Monad.Indexed qualified as Indexed
import Data.Char qualified as Char
import Prelude hiding (Applicative (..), Monad (..))

type PUP = Print Indexed.:*: Indexed.IgnoreStack Parse

parse :: PUP r r' a -> String -> Maybe (a, String)
parse (_ Indexed.:*: Indexed.IgnoreStack prse) = Parse.runParse prse

print :: (Indexed.Unroll r (Maybe String)) => PUP r (Maybe String) a -> r
print (prnt Indexed.:*: _) = Print.print prnt

once :: (r -> r') -> PUP r r' a -> PUP r r' a
once unr (prnt Indexed.:*: Indexed.IgnoreStack prse) = (Print.once unr prnt) Indexed.:*: Indexed.IgnoreStack prse

anyChar :: PUP (Char -> r) r Char
anyChar = Print.anyChar Indexed.:*: Indexed.IgnoreStack Parse.anyChar

char :: Char -> PUP r r ()
char c = Indexed.do
  c' <- anyChar Indexed.@ c
  guard $ c == c'

space :: PUP r r ()
space = char ' '

string :: String -> PUP r r ()
string = mapM_ char

digit :: PUP (Int -> r) r Int
digit = Indexed.do
  Indexed.stack (\_fl k i -> k (head (show i))) (\k _ -> k 0)
  c <- anyChar
  guard $ Char.isDigit c
  Indexed.pure $ read [c]

int :: PUP (Int -> r) r Int
int = Indexed.do
  Indexed.stack digitise (\k -> k . head)
  undigitise <$> Indexed.some digit
  where
    digitise _fl k n = k (digitise' n)
    digitise' n
      | n < 10 = [n]
      | otherwise = let (q, r) = quotRem n 10 in (digitise' q) ++ [r]
    undigitise = foldl (\n d -> 10 * n + d) 0

bool :: PUP (Bool -> r) r Bool
bool =
  trueLead <* string "True"
    <|> falseLead <* string "False"
  where
    trueLead = Indexed.do
      Indexed.stack (\cases _ k True -> k; fl _ b -> fl b) (\k -> k True)
      Indexed.pure True

    falseLead = Indexed.do
      Indexed.stack (\cases _ k False -> k; fl _ b -> fl b) (\k -> k False)
      Indexed.pure False
