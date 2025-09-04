{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines a "Text.Megaparsec" backend for format descriptors. To that
-- effect, this module introduces a type class 'MonadParsec' modeled after
-- Megarparsec's own type class, so that format descriptors can take advantage
-- of Megaparsec features.
--
-- The 'MonadParsec' class in this module isn't exactly the same as the one from
-- Megaparsec (mainly because it needs to be implementable on pretty-printers as
-- well). As such, some of the code in this module is similar or identical to
-- Megaparsec, many of the docstrings are taken directly from Megaparsec as
-- well.
module Text.Pup.Backend.Megaparsec
  ( -- * Concrete backend
    Backend,
    run,
    BackendGen (..),

    -- * Megaparsec-specific format descriptors
    MonadParsec (..),

    -- * Reexports
    module Text.Pup.Class,
  )
where

import Control.Additive
import Control.Applicative
import Control.Monad
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.Kind
import Data.Set (Set)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec.Char
import Text.Pup.Class

type Backend err s = BackendGen (Megaparsec.Parsec err s)

type BackendGen :: (Type -> Type) -> Type -> Type -> Type -> Type
newtype BackendGen m r r' a = MkBackend (Indexed.IgnoreIndices m r r' a)
  deriving newtype
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadPlus,
      Indexed.Applicative,
      Indexed.Monad,
      Cont2.Stacked
    )

deriving newtype instance (Alternative m) => Additive (BackendGen m r r' a)

run :: (Megaparsec.ShowErrorComponent err, Megaparsec.VisualStream s, Megaparsec.TraversableStream s) => Backend err s r r' a -> String -> s -> Either String a
run (MkBackend (Indexed.IgnoreIndices prse)) name input =
  case Megaparsec.runParser prse name input of
    Left err -> Left $ Megaparsec.errorBundlePretty err
    Right a -> Right a

instance (MonadFail m) => Indexed.Fail (BackendGen m) where
  fail msg = MkBackend . Indexed.IgnoreIndices $ fail msg

instance (Megaparsec.MonadParsec err s m, tok ~ Megaparsec.Token s, chunk ~ Megaparsec.Tokens s, Eq tok, Eq chunk) => Tokens tok chunk (BackendGen m) where
  eof = MkBackend . Indexed.IgnoreIndices $ Megaparsec.eof
  satisfy p = MkBackend . Indexed.IgnoreIndices $ Megaparsec.satisfy p
  single t = MkBackend . Indexed.IgnoreIndices $ Megaparsec.single t
  label s (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.label s a
  hidden (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.hidden a
  tokens f s = MkBackend . Indexed.IgnoreIndices $ Megaparsec.tokens f s
  takeWhileP s f = MkBackend . Indexed.IgnoreIndices $ Megaparsec.takeWhileP s f
  takeWhile1P s f = MkBackend . Indexed.IgnoreIndices $ Megaparsec.takeWhile1P s f
  takeP s n = MkBackend . Indexed.IgnoreIndices $ Megaparsec.takeP s n

instance (Megaparsec.MonadParsec err s m, Megaparsec.Token s ~ Char) => Breaks (BackendGen m) where
  space = MkBackend . Indexed.IgnoreIndices $ Megaparsec.Char.space
  space1 = MkBackend . Indexed.IgnoreIndices $ Megaparsec.Char.space1
  hardline = MkBackend . Indexed.IgnoreIndices $ void Megaparsec.Char.newline

deriving via (Trivial (BackendGen m)) instance (Megaparsec.MonadParsec err s m, Megaparsec.Token s ~ Char) => WadlerLeijen (BackendGen m)

instance (Megaparsec.MonadParsec err s m) => LookAhead (BackendGen m) where
  lookAhead (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.lookAhead a
  notFollowedBy (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.notFollowedBy a

-- | A Megaparsec-style class for format descriptors. Inheriting from
-- Megaparsec, it is somewhat parsing-oriented.
--
-- The documentation of individual methods is taken from
-- 'Megaparsec.MonadParsec'.
--
-- Note: some primitive from 'Megaparsec.MonadParsec', in particular
-- reflection/escape hatch primitives, are omitted from this class because they
-- aren't implementable for printers. If you need them, you want to act at the
-- parsec level, not the Pup level.
class (Megaparsec.Stream s, Indexed.MonadPlus m, Cont2.Stacked m, Tokens (Megaparsec.Token s) (Megaparsec.Tokens s) m, LookAhead m) => MonadParsec s m | m -> s where
  -- | The parser @'try' p@ behaves like the parser @p@, except that it
  -- backtracks the parser state when @p@ fails (either consuming input or
  -- not).
  --
  -- This combinator is used whenever arbitrary look ahead is needed. Since
  -- it pretends that it hasn't consumed any input when @p@ fails, the
  -- ('A.<|>') combinator will try its second alternative even if the first
  -- parser failed while consuming input.
  --
  -- For example, here is a parser that is supposed to parse the word “let”
  -- or the word “lexical”:
  --
  -- >>> parseTest (string "let" <|> string "lexical") "lexical"
  -- 1:1:
  -- unexpected "lex"
  -- expecting "let"
  --
  -- What happens here? The first parser consumes “le” and fails (because it
  -- doesn't see a “t”). The second parser, however, isn't tried, since the
  -- first parser has already consumed some input! 'try' fixes this behavior
  -- and allows backtracking to work:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "lexical"
  -- "lexical"
  --
  -- 'try' also improves error messages in case of overlapping alternatives,
  -- because Megaparsec's hint system can be used:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "le"
  -- 1:1:
  -- unexpected "le"
  -- expecting "let" or "lexical"
  --
  -- __Note__ that as of Megaparsec 4.4.0, 'Text.Megaparsec.Char.string'
  -- backtracks automatically (see 'tokens'), so it does not need 'try'.
  -- However, the examples above demonstrate the idea behind 'try' so well
  -- that it was decided to keep them. You still need to use 'try' when your
  -- alternatives are complex, composite parsers.
  try :: m r r' a -> m r r' a

  -- | The parser @'token' test expected@ accepts tokens for which the
  -- matching function @test@ returns 'Just' results. If 'Nothing' is
  -- returned the @expected@ set is used to report the items that were
  -- expected.
  --
  -- For example, the 'Text.Megaparsec.satisfy' parser is implemented as:
  --
  -- > satisfy f = token testToken Set.empty
  -- >   where
  -- >     testToken x = if f x then Just x else Nothing
  token ::
    -- | Matching function for the token to parse
    (Megaparsec.Token s -> Maybe a) ->
    -- | Used in the error message to mention the items that were expected
    Set (Megaparsec.ErrorItem (Megaparsec.Token s)) ->
    -- | Printing function for the token
    (a -> Megaparsec.Token s) ->
    m (a -> r) r a

---------------------------------------------------------------------------
--
-- Building a MonadParsec
--
---------------------------------------------------------------------------

instance (Megaparsec.MonadParsec e s m) => ParseErrors (Megaparsec.ParseError s e) (BackendGen m) where
  parseError = MkBackend . Indexed.IgnoreIndices . Megaparsec.parseError
  withRecovery f (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.withRecovery (Indexed.unIgnoreIndices . (\(MkBackend p) -> p) . f) a
  observing (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.observing (Megaparsec.try a)

instance (Megaparsec.MonadParsec e s m, Megaparsec.Stream s) => MonadParsec s (BackendGen m) where
  try (MkBackend (Indexed.IgnoreIndices a)) = MkBackend . Indexed.IgnoreIndices $ Megaparsec.try a
  token p s _pr = MkBackend . Indexed.IgnoreIndices $ Megaparsec.token p s

instance (MonadParsec s m1, MonadParsec s m2) => MonadParsec s (m1 Indexed.:*: m2) where
  try (a Indexed.:*: b) = try a Indexed.:*: try b
  token p s pr = token p s pr Indexed.:*: token p s pr
