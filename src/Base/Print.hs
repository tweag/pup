{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}

module Base.Print where

import Control.Comonad.Traced qualified as Comonad
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 (Cont2W)
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Prelude hiding (Applicative (..), Monad (..))

type Print = Cont2W (Comonad.Traced String)

print :: forall a b. Print (a -> Maybe String) (Maybe String) b -> a -> Maybe String
print prnt = Cont2.runCont2W prnt (Comonad.traced (\s _ _ -> Just s)) (\_ -> Nothing)

anyChar :: Print (Char -> r) r Char
anyChar = Indexed.do
  c <- Cont2.pop
  Cont2.run' $ Comonad.trace [c]
  Indexed.pure c

once :: (r -> r') -> Print r r' a -> Print r r' a
once unr prnt = Cont2.shift $ \k fl -> Indexed.do
  a <- prnt
  Indexed.pure $ k a (unr fl)
