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
  -- TODO: the printer should error out if the integer on the stack isn't a
  -- single digit.
  Indexed.stack (\_fl k i -> k (head (show i))) (\k _ -> k 0)
  c <- digitChar
  Indexed.pure $ Char.digitToInt c

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
