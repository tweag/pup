{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Test.Simple.Parse where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

newtype Parse a = Parse {runParse :: String -> Maybe (a, String)}
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus) via (StateT String Maybe)

anyChar :: Parse Char
anyChar = Parse $ \cases
  (c : s) -> Just (c, s)
  [] -> Nothing
