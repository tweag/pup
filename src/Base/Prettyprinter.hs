{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module Base.Prettyprinter where

import Base.Megaparsec
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

-- | `e` is a phantom type for the parse error type.
newtype PPrint e ann r r' a = PPrint (Cont2W (Comonad.Traced (Prettyprinter.Doc ann)) r r' a)
  deriving newtype (Functor, Indexed.Applicative, Indexed.Monad, Indexed.Stacked, Cont2.Shifty)

deriving newtype instance Prelude.Applicative (PPrint e ann r r)

deriving newtype instance Alternative (PPrint e ann r r)

deriving newtype instance Prelude.Monad (PPrint e ann r r)

deriving newtype instance MonadPlus (PPrint e ann r r)

run :: forall r a e ann. (Indexed.Unroll r (Maybe (Prettyprinter.Doc ann))) => PPrint e ann r (Maybe (Prettyprinter.Doc ann)) a -> r
run (PPrint prnt) = Cont2.runCont2W prnt (Comonad.traced (\s _ _ -> Just s)) (Indexed.unroll @r @(Maybe (Prettyprinter.Doc ann)) Nothing)

tell :: Prettyprinter.Doc ann -> PPrint e ann r r ()
tell doc = PPrint $ Cont2.run' $ Comonad.trace doc

modify :: (Prettyprinter.Doc ann -> Prettyprinter.Doc ann) -> PPrint e ann r r' a -> PPrint e ann r r' a
modify f (PPrint (Cont2.Cont2W a)) = PPrint $ Cont2.Cont2W $ \(Comonad.TracedT (Identity wk)) fl -> a (Comonad.TracedT (Identity (wk . f))) fl

anyChar :: PPrint e ann (Char -> r) r Char
anyChar = Indexed.do
  c' <- Cont2.pop
  tell (Prettyprinter.pretty c')
  Indexed.pure c'

-- Probably can be generalised beyond text.
instance MonadParsec e Text (PPrint e ann) where
  parseError _ = Indexed.empty
  label _ = id
  try = id -- TODO: probably incorrect
  lookAhead = id -- TODO: probably incorrect
  notFollowedBy = void -- TODO: probably incorrect
  withRecovery _ = id -- TODO: probably incorrect
  observing = (Indexed.>>= return . Right) -- TODO: dubious
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
