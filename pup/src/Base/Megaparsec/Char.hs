{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Format descriptors specialised to the token type 'Char'. See also
-- "Text.Megaparsec.Char".
module Base.Megaparsec.Char
  ( -- * Numbers
    digit,
    nat,

    -- * Individual characters
    char,
    anyChar,
  )
where

import Base.Megaparsec
import Control.Monad.Indexed qualified as Indexed
import Data.Char qualified as Char
import Text.Megaparsec qualified as Megaparsec

-- | Type constrainted version of 'single'
char :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => Char -> m r r Char
char = single

-- | Type constrainted version of 'anyChar'
anyChar :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Char -> r) r Char
anyChar = anySingle

-- | Decimal digit
digit :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Int -> r) r Int
digit = Indexed.do
  Indexed.stack (\_fl k i -> k (head (show i))) (\k _ -> k 0)
  c <- satisfy Char.isDigit <?> "decimal digit"
  Indexed.pure $ read [c]

-- | A (maximal) sequence of decimal digits interpreted as a natural number
nat :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Int -> r) r Int
nat = Indexed.do
  Indexed.shift_ $ \k fl -> Indexed.pure (\i -> k (fl . undigitise) (digitise i))
  undigitise <$> Indexed.some (try digit)
  where
    digitise n
      | n < 10 = [n]
      | otherwise = let (q, r) = quotRem n 10 in (digitise q) ++ [r]
    undigitise = foldl (\n d -> 10 * n + d) 0
