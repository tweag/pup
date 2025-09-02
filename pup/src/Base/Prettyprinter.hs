{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

-- | This module defines a "Prettyprinter" backend for format descriptors. To that
-- effect, this module introduces a type class 'Prettyprinter' with the main
-- primitives that we need (the presentation is a little biased toward parsers,
-- as the shared primitives are bundled with parser type classes). This module
-- also introduces an indexed monad 'PPrint' which is the concrete
-- implementation of the backend.
module Base.Prettyprinter
  ( -- * Type class
    Prettyprinter (..),

    -- * Concrete backend
    PPrint (..),
    run,
    tell,
    modify,
  )
where

import Base.Megaparsec
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
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

-- | `e` is a phantom type for the parse error type. This is required by the
-- functional dependency on 'MonadParsec'.
newtype PPrint e ann r r' a = PPrint (Cont2W (Comonad.Traced (Prettyprinter.Doc ann)) r r' a)
  deriving newtype (Functor, Indexed.Applicative, Indexed.Monad, Cont2.Stacked, Cont2.Shifty, Additive)

deriving newtype instance Prelude.Applicative (PPrint e ann r r)

deriving newtype instance Alternative (PPrint e ann r r)

deriving newtype instance Prelude.Monad (PPrint e ann r r)

deriving newtype instance MonadPlus (PPrint e ann r r)

instance Indexed.Fail (PPrint e ann) where
  fail _msg = Additive.empty

run :: forall a b e ann. PPrint e ann (a -> Maybe (Prettyprinter.Doc ann)) (Maybe (Prettyprinter.Doc ann)) b -> a -> Maybe (Prettyprinter.Doc ann)
run (PPrint prnt) = Cont2.runCont2W prnt (Comonad.traced (\s _ _ -> Just s)) (\_ -> Nothing)

tell :: Prettyprinter.Doc ann -> PPrint e ann r r ()
tell doc = PPrint $ Cont2.yield_ $ Comonad.trace doc

modify :: (Prettyprinter.Doc ann -> Prettyprinter.Doc ann) -> PPrint e ann r r' a -> PPrint e ann r r' a
modify f (PPrint (Cont2.Cont2W a)) = PPrint $ Cont2.Cont2W $ \(Comonad.TracedT (Identity wk)) fl -> a (Comonad.TracedT (Identity (wk . f))) fl

-- Probably can be generalised beyond text.
instance MonadParsec e Text (PPrint e ann) where
  parseError _ = Additive.empty
  label _ = id
  try = id
  lookAhead _ = Cont2.pop
  notFollowedBy _ = Indexed.pure ()
  withRecovery _ = id
  observing pr = Indexed.do
    v <- Cont2.pop
    case v of
      Right b -> Right <$> pr Cont2.@ b
      Left err -> Indexed.pure $ Left err
  eof = Indexed.pure ()
  token _match _expected pr = Indexed.do
    tok <- Cont2.pop
    tell (Prettyprinter.pretty (pr tok))
    Indexed.pure tok
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

-- | A class abstracting over Prettyprinter-like pretty printers.
--
-- The documentation of individual method is adapted from "PrettyPrinter".
class Prettyprinter m where
  -- | @('group' x)@ tries laying out @x@ into a single line by interpreting the
  -- contained line breaks (/e.g./ 'Megaparsec.space1') as spaces; if this does
  -- not fit the page, or when a hardline within @x@ prevents it from being
  -- flattened, @x@ is laid out without any changes.
  group :: m r r' a -> m r r' a

  -- | @('nest' i x)@ lays out the document @x@ with the current nesting level
  -- (indentation of the following lines) increased by @i@. Negative values are
  -- allowed, and decrease the nesting level accordingly.
  --
  -- >>> vsep [nest 4 (vsep ["lorem", "ipsum", "dolor"]), "sit", "amet"]
  -- lorem
  --     ipsum
  --     dolor
  -- sit
  -- amet
  nest :: Int -> m r r' a -> m r r' a

instance Prettyprinter (PPrint e ann) where
  group = modify Prettyprinter.group

  nest n = modify (Prettyprinter.nest n)

instance Prettyprinter (Indexed.IgnoreIndices m) where
  group = id
  nest _n = id

instance (Prettyprinter m1, Prettyprinter m2) => Prettyprinter (m1 Indexed.:*: m2) where
  group (a Indexed.:*: b) = group a Indexed.:*: group b
  nest n (a Indexed.:*: b) = nest n a Indexed.:*: nest n b
