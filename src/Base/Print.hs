{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}

module Base.Print where

import Control.Comonad.Traced qualified as Comonad
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 (Cont2W)
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Prelude hiding (Applicative (..), Monad (..))

type Print = Cont2W (Comonad.Traced String)

print :: forall r a. (Indexed.Unroll r (Maybe String)) => Print r (Maybe String) a -> r
print prnt = Cont2.runCont2W prnt (Comonad.traced (\s _ _ -> Just s)) (Indexed.unroll @r @(Maybe String) Nothing)

anyChar :: Print (Char -> r) r Char
anyChar = Indexed.do
  c <- Cont2.pop
  Cont2.run $ \wk -> Comonad.trace [c] wk c

once :: (r -> r') -> Print r r' a -> Print r r' a
once unr prnt = Cont2.shift $ \wk fl -> Indexed.do
  a <- prnt
  Indexed.pure $ Comonad.extract wk a (unr fl)
