{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}

-- | Format descriptors specialised to the token type 'Char'. See also
-- "Text.Megaparsec.Char".
module Text.Pup.Class.Char
  ( -- * Numbers
    digit,
    digitChar,
    nat,

    -- * Individual characters
    char,
    anyChar,

    -- * Read and show
    read,
    readM,
  )
where

import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.Char qualified as Char
import Text.Pup.Backend.Megaparsec
import Text.Read qualified as Read
import Prelude hiding (read)
import Prelude qualified

-- | Type constrainted version of 'single'
char :: (Tokens Char chunk m) => Char -> m r r Char
char = single

-- | Type constrainted version of 'anyChar'
anyChar :: (Tokens Char chunk m) => m (Char -> r) r Char
anyChar = anySingle

-- | Decimal digit. To manipulate the raw 'Char' instead, use 'digitChar'.
digit :: (Cont2.Stacked m, Indexed.Monad m, Tokens Char chunk m) => m (Int -> r) r Int
digit = Indexed.do
  Cont2.shift_ $ \k fl -> Indexed.pure $ \i -> if 0 <= i && i < 10 then k fl i else fl i
  Cont2.shift_ $ \k fl -> Indexed.pure $ \i -> k (\_ -> fl i) (Char.intToDigit i)
  c <- digitChar
  Indexed.pure $ Char.digitToInt c

-- | A 'Char' standing for a decimal digit. You can return the digit at an 'Int'
-- with 'digit'.
digitChar :: (Tokens Char chunk m) => m (Char -> r) r Char
digitChar =
  satisfy Char.isDigit <?> "decimal digit"

-- | A (maximal) sequence of decimal digits interpreted as a natural number
nat :: (Cont2.Stacked m, Indexed.Alternative m, Tokens Char chunk m) => m (Int -> r) r Int
nat =
  read Indexed.<*> Cont2.some digitChar

-- | A total lead based using 'Prelude.read' and 'show' for the respective directions.
-- It is the responsibility of the parser to ensure that the input is the domain
-- of 'Prelude.read' (the printer, on the other hand always succeeds). Otherwise
-- the 'read' descriptor will fail with 'error'.
--
-- For a format descriptor capable of failing with a parse error, see 'readM'.
read :: (Indexed.Applicative m, Cont2.Stacked m, Read a, Show a) => m (a -> r) (String -> r) (String -> a)
read = Indexed.do
  Cont2.stack (\_fl k a -> k (show a)) (\k s -> k (Prelude.read s))
  Indexed.pure Prelude.read

-- | A format descriptor using 'Prelude.read' and 'show' for the respective
-- directions. If 'read' fails, then a parse error is reported (with the same
-- message as 'readEither'). In exchange 'readM', compared to 'read', must use a
-- monadic control flow.
readM :: (Indexed.MonadFail m, Cont2.Stacked m, Read a, Show a) => String -> m (a -> r) (String -> r) a
readM s = Indexed.do
  Cont2.stack (\_fl k a -> k (show a)) (\k s' -> k (Prelude.read s'))
  case Read.readEither s of
    Right a -> Indexed.pure a
    Left err -> Indexed.fail err
