{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Combinators where

import Base.Parse (Parse (..))
import Base.Parse qualified as Parse
import Base.Print (Print (..))
import Base.Print qualified as Print
import Control.Monad.Indexed ((<*), (<|>))
import Control.Monad.Indexed qualified as Indexed
import Data.Char qualified as Char
import Prelude hiding (Applicative (..), Monad (..))

type PUP = Print Indexed.:*: Indexed.IgnoreStack Parse

parse :: PUP rf r r' a -> String -> Maybe (a, String)
parse (_ Indexed.:*: Indexed.IgnoreStack prse) = Parse.runParse prse

print :: PUP (Maybe String) r (Maybe String) a -> r
print (prnt Indexed.:*: _) = Print.print prnt

anyChar :: PUP rf (Char -> r) r Char
anyChar = Print.anyChar Indexed.:*: Indexed.IgnoreStack Parse.anyChar

char :: Char -> PUP r r r ()
char c = Indexed.do
  Indexed.stack $ \_ k -> k c
  c' <- anyChar
  Indexed.guard $ c == c'

string :: String -> PUP r r r ()
string = mapM_ char

digit :: PUP r (Int -> r) r Int
digit = Indexed.do
  Indexed.stack $ \_fl k i -> k (head (show i))
  c <- anyChar
  Indexed.guard $ Char.isDigit c
  Indexed.pure $ read [c]

-- int :: PUP r (Int -> r) r Int
-- int = Indexed.do
--   Indexed.stack digitise
--   undigitise <$> Indexed.handle (\_ -> id) (Indexed.some digit)
--   where
--     -- digitise :: _ -> ([Int] -> _) -> Int -> _
--     digitise fl k n
--       | n < 10 = k [n]
--       | otherwise = let (q,r) = quotRem n 10 in digitise fl (\ds -> k (ds ++ [r])) q
--     undigitise = foldl (\n d -> 10*n + d) 0

-- bool :: PUP r (Bool -> r) r Bool
-- bool =
--     trueLead <* string "True"
--     <|> falseLead <* string "False"
--   where
--     trueLead = Indexed.do
--       Indexed.stack (\cases {_ k True -> k; fl _ _ -> fl})
--       Indexed.pure True

--     falseLead = Indexed.do
--       Indexed.stack (\cases {_ k False -> k; fl _ _ -> fl})
--       Indexed.pure False
