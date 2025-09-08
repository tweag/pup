{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QualifiedDo #-}

-- | This module defines a pup with a
-- [Megaparsec](https://hackage.haskell.org/package/megaparsec) backend for the
-- parser side and a
-- [Prettyprinter](https://hackage.haskell.org/package/prettyprinter) backend
-- for the parser.
--
-- Start with 'Pup'' if you don't have any advanced need, change to the more
-- general 'Pup' if you want pretty-printing annotations or custom parse error.
module Text.Pup.MPR
  ( -- * A simple pup to get you started
    Pup',
    print,
    parse,

    -- * More general version
    Pup (..),

    -- * Re-exports
    module Text.Pup.Class,
    module Text.Pup.Class.Char,
    Megaparsec.MonadParsec (..),
  )
where

import Control.Additive
import Control.Applicative
import Control.Monad
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.String qualified as String
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter qualified
import Text.Megaparsec qualified as Text.Megaparsec
import Text.Pup.Backend.Megaparsec qualified as Megaparsec
import Text.Pup.Backend.Prettyprinter qualified as Print
import Text.Pup.Class
import Text.Pup.Class.Char
import Prelude hiding (print)

-- | The more general type of Megaparsec+Prettyprinter pup. You can still use
-- 'print' and 'parse' with it.
newtype Pup err ann r r' a = MkPup ((Print.Backend ann Indexed.:*: Megaparsec.Backend err Text) r r' a)
  deriving newtype
    ( Functor,
      Indexed.Applicative,
      Indexed.Monad,
      Indexed.Fail,
      Cont2.Stacked,
      Additive,
      Tokens Char Text,
      Breaks,
      WadlerLeijen,
      LookAhead,
      ParseErrors (Text.Megaparsec.ParseError Text err),
      Megaparsec.MonadParsec Text
    )

deriving newtype instance Applicative (Pup err ann r r)

deriving newtype instance (Ord err) => Alternative (Pup err ann r r)

deriving newtype instance Monad (Pup err ann r r)

deriving newtype instance (Ord err) => MonadPlus (Pup err ann r r)

instance (Ord err, r ~ r', a ~ ()) => String.IsString (Pup err ann r r' a) where
  fromString = chunk . String.fromString

type Pup' = Pup Void ()

print :: Pup err ann (a -> Maybe (Prettyprinter.Doc ann)) (Maybe (Prettyprinter.Doc ann)) b -> a -> Maybe (Prettyprinter.Doc ann)
print (MkPup (prnt Indexed.:*: _)) = Print.run prnt

parse :: (Text.Megaparsec.ShowErrorComponent err) => Pup err ann r r' a -> String -> Text -> Either String a
parse (MkPup (_ Indexed.:*: prse)) = Megaparsec.run prse
