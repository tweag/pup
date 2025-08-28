{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Format descriptors specialised to the token type 'Char'. See also
-- "Text.Megaparsec.Char".
module Base.Megaparsec.Char
  ( -- * Numbers
    digit,
    digitChar,
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

-- | Decimal digit. To manipulate the raw 'Char' instead, use 'digitChar'.
digit :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Int -> r) r Int
digit = Indexed.do
  Indexed.stack decimal (\k _ -> k 0)
  c <- digitChar
  Indexed.pure $ Char.digitToInt c
  where
    decimal :: (Int -> r) -> (Char -> r) -> Int -> r
    decimal fl k i
      | 0 <= i && i < 10 = k (Char.intToDigit i)
      | otherwise = fl i

-- | A 'Char' standing for a decimal digit. You can return the digit at an 'Int'
-- with 'digit'.
digitChar :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Char -> r) r Char
digitChar = Indexed.do
  satisfy Char.isDigit <?> "decimal digit"

-- | A (maximal) sequence of decimal digits interpreted as a natural number
nat :: (MonadParsec e s m, Megaparsec.Token s ~ Char) => m (Int -> r) r Int
nat = Indexed.do
  Indexed.shift_ $ \k fl -> Indexed.pure (\i -> k (fl . read) (show i))
  read <$> Indexed.some digitChar
