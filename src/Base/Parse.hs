{-# LANGUAGE DerivingVia #-}

module Base.Parse where

import Control.Monad.State.Strict

newtype Parse a = Parse {runParse :: String -> Maybe (a, String)}
  deriving (Functor, Applicative, Monad) via (StateT String Maybe)
