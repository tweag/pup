{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Base.Parse where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad

newtype Parse a = Parse {runParse :: String -> Maybe (a, String)}
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus) via (StateT String Maybe)

anyChar :: Parse Char
anyChar = Parse $ \cases
  (c:s) -> Just (c ,s)
  [] -> Nothing
