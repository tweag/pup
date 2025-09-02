{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Best
  ( tests,
    reprintSexpr,
    a_small_sexpr,
    a_sexpr,
  )
where

import Base.Megaparsec
import Base.Megaparsec.Char
import Base.Prettyprinter
import BestPUP
import Control.Additive ((<|>))
import Control.Monad hiding (guard)
import Control.Monad.Indexed ((<*), (<*>))
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Control.Monad.Indexed.Cont2.Lead.Generic (lead)
import Control.Monad.Indexed.Cont2.Lead.Labels ()
import Data.Maybe qualified as Maybe
import Data.String qualified as String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter
import Text.Megaparsec qualified as Megaparsec
import Prelude hiding (Applicative (..), Monad (..))
import Prelude qualified

-------------------------------------------------------------------------------
--
-- Additional parsers
--
-------------------------------------------------------------------------------

bool :: (MonadParsec e s m, String.IsString (Megaparsec.Tokens s)) => m (Bool -> r) r Bool
bool =
  #True <* chunk "True"
    <|> #False <* chunk "False"

-------------------------------------------------------------------------------
--
-- Generic properties of pups
--
-------------------------------------------------------------------------------

roundTrip :: forall m a. (Monad m, Eq a, Show a) => Pup' (a -> Maybe (Prettyprinter.Doc ())) (Maybe (Prettyprinter.Doc ())) a -> a -> PropertyT m ()
roundTrip pp a = do
  doc <- shouldPrint $ BestPUP.print pp a
  let str = Prettyprinter.renderStrict $ Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions doc
  Hedgehog.annotate $ "The printed doc: " ++ (Text.unpack $ str)
  BestPUP.parse pp "<test>" str `shouldParseTo` a
  where
    shouldPrint :: forall x. Maybe x -> PropertyT m x
    shouldPrint (Just x) = return x
    shouldPrint Nothing = Prelude.fail "Couldn't print"

    shouldParseTo :: forall x. (Eq x, Show x) => Either String x -> x -> PropertyT m ()
    shouldParseTo (Left err) _ = Prelude.fail $ "Failed to parse: " ++ err
    shouldParseTo (Right x) y = x === y

-------------------------------------------------------------------------------
--
-- Simple pups
--
-------------------------------------------------------------------------------

-- TODO: a lot could be factored between here and the Simple pup tests

data T = C Int Bool | D Char Bool Int
  deriving (Show, Generic, Eq)

genT :: (MonadGen m) => m T
genT = Gen.choice [genC, genD]
  where
    genC = C Prelude.<$> Gen.int (Range.linear 0 100) Prelude.<*> Gen.bool
    genD = D Prelude.<$> Gen.alphaNum Prelude.<*> Gen.bool Prelude.<*> Gen.int (Range.linear 0 100)

uupT :: Pup' (T -> r) r T
uupT =
  #C <* chunk "C" <* space1 <*> nat <* space1 <*> bool
    <|> #D <* chunk "D" <* space1 <*> anySingle <* space1 <*> bool <* space1 <*> nat

data U = K T Int | L
  deriving (Show, Generic, Eq)

genU :: (MonadGen m) => m U
genU = Gen.frequency [(20, genK), (1, genL)]
  where
    genK = K Prelude.<$> genT Prelude.<*> Gen.int (Range.linear 0 100)
    genL = return L

uupU :: Pup' (U -> r) r U
uupU =
  #K <* chunk "K" <* space1 <*> uupT <* space1 <*> nat
    <|> #L <* chunk "L"

prop_round_trip_Bool :: Property
prop_round_trip_Bool = property $ do
  x <- forAll $ Gen.bool
  roundTrip bool x

prop_round_trip_Digit :: Property
prop_round_trip_Digit = property $ do
  x <- forAll $ Gen.int (Range.linear 0 9)
  roundTrip digit x

prop_round_trip_Nat :: Property
prop_round_trip_Nat = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  roundTrip nat x

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
-- Pupping Sexprs
--
-------------------------------------------------------------------------------

data SExpr
  = SList [SExpr]
  | SSymb String
  | SStr Text
  | SInt Int
  deriving (Generic, Show, Eq)

-- A simple s-expr parser, doesn't handle escaped characters in strings
sexpr :: Pup' (SExpr -> r) r SExpr
sexpr =
  group (nest 2 (#SList <* try (chunk "(") <* space <*> try sexpr `Cont2.sepBy` space1 <* space <* chunk ")"))
    <|> #SSymb <*> try symbol
    <|> #SInt <*> try nat
    <|> #SStr <* try (chunk "\"") <*> takeWhileP Nothing (/= '"') <* chunk "\""
  where
    symbol :: forall r'. Pup' (String -> r') r' String
    symbol = lead @":" <*> symbol_lead <*> Cont2.many (try symbol_other)

    symbol_lead :: forall r'. Pup' (Char -> r') r' Char
    symbol_lead = oneOf (':' : ['a' .. 'z'] ++ ['A' .. 'Z'])
    symbol_other :: forall r'. Pup' (Char -> r') r' Char
    symbol_other = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-'])

reprintSexpr :: Text -> IO ()
reprintSexpr str = do
  case parse sexpr "<test>" str of
    Left e -> putStrLn e
    Right expr -> do
      let doc = Maybe.fromJust (BestPUP.print sexpr expr)
      let str' = Prettyprinter.renderStrict $ Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions doc
      putStrLn $ Text.unpack str'
  putStrLn ""

a_small_sexpr :: Text
a_small_sexpr = "((abstr 57 :tag) \"this is nested\")"

-- Modified Emacs Lisp
a_sexpr :: Text
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
      SStr Prelude.<$> (Text.pack <$> Gen.string (Range.linear 0 30) Gen.alphaNum)
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
-- Collecting tests
--
-------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$(discover)
