{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog.Main qualified as Hedgehog
import Test.Simple qualified as Simple
import Test.Best qualified as Best
-------------------------------------------------------------------------------
--
-- Running tests
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== Visualise simple PUP =="
  Simple.reprintSexpr Simple.a_small_sexpr
  Simple.reprintSexpr Simple.a_sexpr
  putStrLn "== Visualise megaparsec/prettyprinter PUP =="
  Best.reprintSexpr Best.a_small_sexpr
  Best.reprintSexpr Best.a_sexpr
  Hedgehog.defaultMain [Simple.tests, Best.tests]
