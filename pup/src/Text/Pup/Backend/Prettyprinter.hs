{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}

-- | This module defines a "Prettyprinter" backend for format descriptors.
module Text.Pup.Backend.Prettyprinter
  ( -- * Concrete backend
    Backend (..),
    run,
    tell,
    modify,

    -- * Re-exports
    module Text.Pup.Class,
  )
where

import Control.Additive (Additive)
import Control.Additive qualified as Additive
import Control.Applicative
import Control.Comonad.Identity
import Control.Comonad.Traced qualified as Comonad
import Control.Monad
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 (Cont2W)
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.Text (Text)
import Prettyprinter qualified
import Text.Pup.Backend.Megaparsec qualified as Megaparsec
import Text.Pup.Class
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

-- | `e` is a phantom type for the parse error type. This is required by the
-- functional dependency on 'MonadParsec'.
newtype Backend ann r r' a = Backend (Cont2W (Comonad.Traced (Prettyprinter.Doc ann)) r r' a)
  deriving newtype (Functor, Indexed.Applicative, Indexed.Monad, Cont2.Stacked, Cont2.Shifty, Additive)
  deriving (LookAhead) via (Trivial (Backend ann))

deriving newtype instance Prelude.Applicative (Backend ann r r)

deriving newtype instance Alternative (Backend ann r r)

deriving newtype instance Prelude.Monad (Backend ann r r)

deriving newtype instance MonadPlus (Backend ann r r)

instance Indexed.Fail (Backend ann) where
  fail _msg = Additive.empty

run :: forall a b ann. Backend ann (a -> Maybe (Prettyprinter.Doc ann)) (Maybe (Prettyprinter.Doc ann)) b -> a -> Maybe (Prettyprinter.Doc ann)
run (Backend prnt) = Cont2.runCont2W prnt (Comonad.traced (\s _ _ -> Just s)) (\_ -> Nothing)

tell :: Prettyprinter.Doc ann -> Backend ann r r ()
tell doc = Backend $ Cont2.yield_ $ Comonad.trace doc

modify :: (Prettyprinter.Doc ann -> Prettyprinter.Doc ann) -> Backend ann r r' a -> Backend ann r r' a
modify f (Backend (Cont2.Cont2W a)) = Backend $ Cont2.Cont2W $ \(Comonad.TracedT (Identity wk)) fl -> a (Comonad.TracedT (Identity (wk . f))) fl

instance Tokens Char Text (Backend ann) where
  label _ = id
  eof = Indexed.pure ()
  satisfy p = Indexed.do
    tok <- Cont2.pop
    guard (p tok)
    tell (Prettyprinter.pretty tok)
    Indexed.pure tok
  single t = Indexed.do
    tell (Prettyprinter.pretty t)
    Indexed.pure t
  tokens _matches _chunk = Indexed.do
    toks <- Cont2.pop
    tell (Prettyprinter.pretty toks)
    Indexed.pure toks
  takeP _label _n = Indexed.do
    toks <- Cont2.pop
    tell (Prettyprinter.pretty toks)
    Indexed.pure toks
  takeWhileP _label _pred = Indexed.do
    toks <- Cont2.pop
    tell (Prettyprinter.pretty toks)
    Indexed.pure toks
  takeWhile1P _label _pred = Indexed.do
    toks <- Cont2.pop
    tell (Prettyprinter.pretty toks)
    Indexed.pure toks

instance Breaks (Backend ann) where
  space = Indexed.pure ()
  space1 = tell Prettyprinter.line
  hardline = tell Prettyprinter.hardline

instance ParseErrors e' (Backend ann) where
  parseError _ = Additive.empty
  withRecovery _ = id
  observing pr = Indexed.do
    v <- Cont2.pop
    case v of
      Right b -> Right <$> pr Cont2.@ b
      Left err -> Indexed.pure $ Left err

instance Megaparsec.MonadParsec Text (Backend ann) where
  try = id
  token _match _expected pr = Indexed.do
    tok <- Cont2.pop
    tell (Prettyprinter.pretty (pr tok))
    Indexed.pure tok

instance WadlerLeijen (Backend ann) where
  group = modify Prettyprinter.group

  nest n = modify (Prettyprinter.nest n)
