{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Combinators
import Control.Monad
import Control.Monad.Indexed ((<*), (<*>), (<|>))
import Control.Monad.Indexed qualified as Indexed
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import GHC.Generics
import Generic (lead)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main qualified as Hedgehog
import Hedgehog.Range qualified as Range
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

-------------------------------------------------------------------------------
--
-- Generic properties of pups
--
-------------------------------------------------------------------------------

roundTrip :: (Monad m, Eq a, Show a) => PUP (a -> Maybe String) (Maybe String) a -> a -> PropertyT m ()
roundTrip pp a =
  case Combinators.print pp a of
    Nothing -> do
      Hedgehog.annotate "Couldn't print"
      Nothing === Just (a, "")
    Just str -> do
      Hedgehog.annotate $ "The printed string: " ++ str
      Combinators.parse pp str === Just (a, "")

-------------------------------------------------------------------------------
--
-- Some basic data type pups
--
-------------------------------------------------------------------------------

data T = C Int Bool | D Char Bool Int
  deriving (Show, Generic, Eq)

genT :: (MonadGen m) => m T
genT = Gen.choice [genC, genD]
  where
    genC = C Prelude.<$> Gen.int (Range.linear 0 100) Prelude.<*> Gen.bool
    genD = D Prelude.<$> Gen.alphaNum Prelude.<*> Gen.bool Prelude.<*> Gen.int (Range.linear 0 100)

uupT :: PUP (T -> r) r T
uupT =
  lead @"C" <* string "C" <* space <*> int <* space <*> bool
    <|> lead @"D" <* string "D" <* space <*> anyChar <* space <*> bool <* space <*> int

data U = K T Int | L
  deriving (Show, Generic, Eq)

genU :: (MonadGen m) => m U
genU = Gen.frequency [(20, genK), (1, genL)]
  where
    genK = K Prelude.<$> genT Prelude.<*> Gen.int (Range.linear 0 100)
    genL = return L

uupU :: PUP (U -> r) r U
uupU =
  lead @"K" <* string "K" <* space <*> uupT <* space <*> int
    <|> lead @"L" <* string "L"

prop_round_trip_T :: Property
prop_round_trip_T = property $ do
  x <- forAll genT
  roundTrip uupT x

prop_round_trip_U :: Property
prop_round_trip_U = property $ do
  x <- forAll genU
  roundTrip uupU x

-------------------------------------------------------------------------------
--
-- Backtracking behaviour
--
-------------------------------------------------------------------------------

-- An example of backtracking behaviour
bktrk :: PUP r r ()
bktrk = Indexed.do
  b <- Indexed.pure True <|> Indexed.pure False
  guard $ not b
  case b of
    True -> string "True"
    False -> string "False"

prop_print_bktrk :: Property
prop_print_bktrk =
  property $
    Combinators.print bktrk === Just "False"

-- Why does (<|>) backtrack in Print. Because:
--
-- pure True <|> pure False = Print $ \fl k -> k True "" (k False "" fl)
--
-- so
--
-- (pure True <|> pure False) >>= f = Print $ \fl k ->
--   let k x sk flx = runPrint (f x) flx $ \y sy fly -> k y (sx ++ sy) fly in
--   k True "" (k False "" fl)

-- Avoiding backtracking
bktrk_once :: Bool -> PUP r r ()
bktrk_once b0 = Indexed.do
  b <- Combinators.once id (Indexed.pure True <|> Indexed.pure False)
  guard $ b == b0
  case b of
    True -> string "True"
    False -> string "False"

prop_print_bktrk_once :: Property
prop_print_bktrk_once = property $ do
  Combinators.print (bktrk_once True) === Just "True"
  Combinators.print (bktrk_once False) === Nothing

-------------------------------------------------------------------------------
--
-- Pupping Sexprs
--
-------------------------------------------------------------------------------

data SExpr
  = SList [SExpr]
  | SSymb String
  | SStr String
  | SInt Int
  deriving (Generic, Show, Eq)

whitespace :: PUP (Char -> r) r Char
whitespace = Indexed.do
  c <- anyChar
  guard $ List.elem c [' ', '\n']
  Indexed.pure c

ws :: PUP r r ()
ws = void $ Indexed.many whitespace Indexed.@ " "

puntil :: (a -> Bool) -> (forall r'. PUP (a -> r') r' a) -> PUP ([a] -> r) r [a]
puntil p pup = Indexed.many $ Indexed.do
  a <- pup
  guard $ not (p a)
  Indexed.pure a

-- A simple s-expr parser, doesn't handle escaped characters in strings
sexpr :: PUP (SExpr -> r) r SExpr
sexpr =
  lead @"SList" <* string "(" <* ws <*> Indexed.many (sexpr <* ws) <* string ")" <* ws
    <|> lead @"SSymb" <*> symbol
    <|> lead @"SInt" <*> int
    <|> lead @"SStr" <* string "\"" <*> puntil (== '"') anyChar <* string "\""
  where
    symbol :: forall r'. PUP (String -> r') r' String
    symbol = lead @":" <*> symbol_lead <*> Indexed.many symbol_other

    symbol_lead :: forall r'. PUP (Char -> r') r' Char
    symbol_lead = Indexed.do
      c <- anyChar
      guard $ List.elem c $ ':' : ['a' .. 'z'] ++ ['A' .. 'Z']
      Indexed.pure c
    symbol_other :: forall r'. PUP (Char -> r') r' Char
    symbol_other = Indexed.do
      c <- anyChar
      guard $ List.elem c $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-']
      Indexed.pure c

reprintSexpr :: String -> IO ()
reprintSexpr str = do
  case Combinators.parse sexpr str of
    Nothing -> putStrLn "Sexp: parse error"
    Just (expr, _) -> putStrLn $ Maybe.fromJust (Combinators.print sexpr expr)
  putStrLn ""

a_small_sexpr :: String
a_small_sexpr = "((abstr 57 :tag) \"this is nested\")"

-- Modified Emacs Lisp
a_sexpr :: String
a_sexpr =
  "(ert-deftest company-shows-keywords-alongside-completions-alphabetically () \
  \  :tags (company) \
  \  (switch-to-buffer \"*TESTING COMPANY MODE ~ Python*\") \
  \  (python-mode) \
  \  \
  \  \
  \  (erase-buffer)\
  \  (insert \" def first(x): pass\")\
  \  (insert \" def fierce(a, b): pass\")\
  \  \
  \  \
  \  (insert \" fi\")\
  \  (company-manual-begin)\
  \  (should (equal company-candidates (\"fierce\" \"first\" (\"finally\" 0 7 (company-backend company-keywords)))))\
  \  \
  \  \
  \  (execute-kbd-macro (kbd \"C-g C-/ M-2\"))\
  \  (should (looking-back \"finally\"))\
  \  \
  \  (kill-buffer))"

genSexpr :: (MonadGen m) => m SExpr
genSexpr =
  Gen.recursive
    Gen.choice
    [ SSymb Prelude.<$> genSymb,
      SInt Prelude.<$> Gen.int (Range.linear 0 100),
      SStr Prelude.<$> Gen.string (Range.linear 0 30) Gen.alphaNum
    ]
    [ SList Prelude.<$> Gen.list (Range.linear 0 7) genSexpr
    ]
  where
    genSymb = (:) <$> Gen.alpha Prelude.<*> Gen.string (Range.linear 0 15) Gen.alphaNum

prop_round_trip_sexpr :: Property
prop_round_trip_sexpr = property $ do
  x <- forAll genSexpr
  roundTrip sexpr x

-------------------------------------------------------------------------------
--
-- Running tests
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
  reprintSexpr a_small_sexpr
  reprintSexpr a_sexpr
  Hedgehog.defaultMain [tests]

tests :: IO Bool
tests = checkParallel $$(discover)
