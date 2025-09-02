{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines a Megaparsec backend for format descriptors. To that
-- effect, this module introduces a type class 'MonadParsec' modeled after
-- Megarparsec's own type class, so that format descriptors can take advantage
-- of Megaparsec features.
--
-- The 'MonadParsec' class in this module isn't exactly the same as the one from
-- Megaparsec (mainly because it needs to be implementable on pretty-printers as
-- well). As such, some of the code in this module is similar or identical to
-- Megaparsec, many of the docstrings are taken directly from Megaparsec as
-- well.
module Base.Megaparsec
  ( -- * The parser type class
    MonadParsec (..),

    -- * Derived parser combinators
    single,
    chunk,
    satisfy,
    oneOf,
    noneOf,
    anySingle,
    anySingleBut,
    (<?>),
    takeRest,
    atEnd,
  )
where

import Control.Additive
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec qualified as Megaparsec

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
class (Megaparsec.Stream s, Indexed.MonadPlus m, Cont2.Stacked m) => MonadParsec e s m | m -> e s where
  -- | Stop parsing and report the 'ParseError'. This is the only way to
  -- control position of the error without manipulating the parser state
  -- manually.
  parseError :: Megaparsec.ParseError s e -> m r r' a

  -- | The parser @'label' name p@ behaves as parser @p@, but whenever the
  -- parser @p@ fails /without consuming any input/, it replaces names of
  -- “expected” tokens with the name @name@.
  label :: String -> m r r' a -> m r r' a

  -- | @'hidden' p@ behaves just like parser @p@, but it doesn't show any
  -- “expected” tokens in error message when @p@ fails.
  --
  -- Please use 'hidden' instead of the old @'label' ""@ idiom.
  hidden :: m r r' a -> m r r' a
  hidden = label ""

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

  -- | If @p@ in @'lookAhead' p@ succeeds (either consuming input or not)
  -- the whole parser behaves like @p@ succeeded without consuming anything
  -- (parser state is not updated as well). If @p@ fails, contrary to
  -- Megaparsec, @'lookAhead' p@ fails without consuming input.
  lookAhead :: m (a -> r) r a -> m (a -> r) r a

  -- | @'notFollowedBy' p@ only succeeds when the parser @p@ fails. This
  -- parser /never consumes/ any input and /never modifies/ parser state. It
  -- can be used to implement the “longest match” rule.
  notFollowedBy :: m r r a -> m r r ()

  -- | @'withRecovery' r p@ allows us to continue parsing even if the parser
  -- @p@ fails. In this case @r@ is called with the actual 'ParseError' as
  -- its argument. Typical usage is to return a value signifying failure to
  -- parse this particular object and to consume some part of the input up
  -- to the point where the next object starts.
  --
  -- Note that if @r@ fails, the original error message is reported as if
  -- without 'withRecovery'. In no way recovering parser @r@ can influence
  -- error messages.
  withRecovery ::
    -- | How to recover from failure
    (Megaparsec.ParseError s e -> m r r' a) ->
    -- | Original parser
    m r r' a ->
    -- | Parser that can recover from failures
    m r r' a

  -- | @'observing' p@ allows us to “observe” failure of the @p@ parser,
  -- should it happen, without actually ending parsing but instead getting
  -- the 'ParseError' in 'Left', and no input is consumed. On success parsed value is returned in
  -- 'Right' as usual.
  observing ::
    -- | The parser to run
    m (b -> r) r a ->
    m ((Either (Megaparsec.ParseError s e) b) -> r) r (Either (Megaparsec.ParseError s e) a)

  -- | This parser only succeeds at the end of input.
  eof :: m r r ()

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

  -- | The parser @'tokens' test chk@ parses a chunk of input @chk@ and
  -- returns it. The supplied predicate @test@ is used to check equality of
  -- given and parsed chunks after a candidate chunk of correct length is
  -- fetched from the stream.
  --
  -- This can be used for example to write 'Text.Megaparsec.chunk':
  --
  -- > chunk = tokens (==)
  --
  -- Note that beginning from Megaparsec 4.4.0, this is an auto-backtracking
  -- primitive, which means that if it fails, it never consumes any input.
  -- This is done to make its consumption model match how error messages for
  -- this primitive are reported (which becomes an important thing as user
  -- gets more control with primitives like 'withRecovery'):
  --
  -- >>> parseTest (string "abc") "abd"
  -- 1:1:
  -- unexpected "abd"
  -- expecting "abc"
  --
  -- This means, in particular, that it's not necessary to use 'try' with
  -- 'tokens'-based parsers, such as 'chunk'. This feature /does not/ affect
  -- performance in any way.
  tokens ::
    -- | Predicate to check equality of chunks
    (Megaparsec.Tokens s -> Megaparsec.Tokens s -> Bool) ->
    -- | Chunk of input to match against
    Megaparsec.Tokens s ->
    m (Megaparsec.Tokens s -> r) r (Megaparsec.Tokens s)

  -- | Parse /zero/ or more tokens for which the supplied predicate holds.
  -- Try to use this as much as possible because for many streams this
  -- combinator is much faster than parsers built with
  -- 'Control.Monad.Combinators.many' and 'Text.Megaparsec.satisfy'.
  --
  -- > takeWhileP (Just "foo") f = many (satisfy f <?> "foo")
  -- > takeWhileP Nothing      f = many (satisfy f)
  --
  -- The combinator never fails, although it may parse the empty chunk.
  takeWhileP ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | Predicate to use to test tokens
    (Megaparsec.Token s -> Bool) ->
    -- | A chunk of matching tokens
    m (Megaparsec.Tokens s -> r) r (Megaparsec.Tokens s)

  -- | Similar to 'takeWhileP', but fails if it can't parse at least one
  -- token. Try to use this as much as possible because for many streams
  -- this combinator is much faster than parsers built with
  -- 'Control.Monad.Combinators.some' and 'Text.Megaparsec.satisfy'.
  --
  -- > takeWhile1P (Just "foo") f = some (satisfy f <?> "foo")
  -- > takeWhile1P Nothing      f = some (satisfy f)
  --
  -- Note that the combinator either succeeds or fails without consuming any
  -- input, so 'try' is not necessary with it.
  takeWhile1P ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | Predicate to use to test tokens
    (Megaparsec.Token s -> Bool) ->
    -- | A chunk of matching tokens
    m (Megaparsec.Tokens s -> r) r (Megaparsec.Tokens s)

  -- | Extract the specified number of tokens from the input stream and
  -- return them packed as a chunk of stream. If there is not enough tokens
  -- in the stream, a parse error will be signaled. It's guaranteed that if
  -- the parser succeeds, the requested number of tokens will be returned.
  --
  -- The parser is roughly equivalent to:
  --
  -- > takeP (Just "foo") n = count n (anySingle <?> "foo")
  -- > takeP Nothing      n = count n anySingle
  --
  -- Note that if the combinator fails due to insufficient number of tokens
  -- in the input stream, it backtracks automatically. No 'try' is necessary
  -- with 'takeP'.
  takeP ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | How many tokens to extract
    Int ->
    -- | A chunk of matching tokens
    m (Megaparsec.Tokens s -> r) r (Megaparsec.Tokens s)

---------------------------------------------------------------------------
--
-- Derived operations. Adapted from Megaparsec
--
---------------------------------------------------------------------------

-- | @'single' t@ only matches the single token @t@.
--
-- > semicolon = single ';'
--
-- See also: 'token', 'anySingle', 'Text.Megaparsec.Byte.char',
-- 'Text.Megaparsec.Char.char'.
single ::
  (MonadParsec e s m) =>
  -- | Token to match
  Megaparsec.Token s ->
  m r r (Megaparsec.Token s)
single t = token testToken expected id Cont2.@ t
  where
    testToken x = if x == t then Just x else Nothing
    expected = Set.singleton (Megaparsec.Tokens (t NonEmpty.:| []))
{-# INLINE single #-}

-- | The parser @'satisfy' f@ succeeds for any token for which the supplied
-- function @f@ returns 'True'.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)
--
-- __Performance note__: when you need to parse a single token, it is often
-- a good idea to use 'satisfy' with the right predicate function instead of
-- creating a complex parser using the combinators.
--
-- See also: 'anySingle', 'anySingleBut', 'oneOf', 'noneOf'.
satisfy ::
  (MonadParsec e s m) =>
  -- | Predicate to apply
  (Megaparsec.Token s -> Bool) ->
  m (Megaparsec.Token s -> r) r (Megaparsec.Token s)
satisfy f = token testChar Set.empty id
  where
    testChar x = if f x then Just x else Nothing
{-# INLINE satisfy #-}

-- | Parse and return a single token. It's a good idea to attach a 'label'
-- to this parser.
--
-- > anySingle = satisfy (const True)
--
-- See also: 'satisfy', 'anySingleBut'.
anySingle :: (MonadParsec e s m) => m (Megaparsec.Token s -> r) r (Megaparsec.Token s)
anySingle = satisfy (const True)
{-# INLINE anySingle #-}

-- | Match any token but the given one. It's a good idea to attach a 'label'
-- to this parser.
--
-- > anySingleBut t = satisfy (/= t)
--
-- See also: 'single', 'anySingle', 'satisfy'.
anySingleBut ::
  (MonadParsec e s m) =>
  -- | Token we should not match
  Megaparsec.Token s ->
  m (Megaparsec.Token s -> r) r (Megaparsec.Token s)
anySingleBut t = satisfy (/= t)
{-# INLINE anySingleBut #-}

-- | @'oneOf' ts@ succeeds if the current token is in the supplied
-- collection of tokens @ts@. Returns the parsed token. Note that this
-- parser cannot automatically generate the “expected” component of error
-- message, so usually you should label it manually with 'label' or ('<?>').
--
-- > oneOf cs = satisfy (`elem` cs)
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"
--
-- __Performance note__: prefer 'satisfy' when you can because it's faster
-- when you have only a couple of tokens to compare to:
--
-- > quoteFast = satisfy (\x -> x == '\'' || x == '\"')
-- > quoteSlow = oneOf "'\""
oneOf ::
  (Foldable f, MonadParsec e s m) =>
  -- | Collection of matching tokens
  f (Megaparsec.Token s) ->
  m (Megaparsec.Token s -> r) r (Megaparsec.Token s)
oneOf cs = satisfy (\x -> elem x cs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @'noneOf' ts@ succeeds if the current token
-- /not/ in the supplied list of tokens @ts@. Returns the parsed character.
-- Note that this parser cannot automatically generate the “expected”
-- component of error message, so usually you should label it manually with
-- 'label' or ('<?>').
--
-- > noneOf cs = satisfy (`notElem` cs)
--
-- See also: 'satisfy'.
--
-- __Performance note__: prefer 'satisfy' and 'anySingleBut' when you can
-- because it's faster.
noneOf ::
  (Foldable f, MonadParsec e s m) =>
  -- | Collection of taken we should not match
  f (Megaparsec.Token s) ->
  m (Megaparsec.Token s -> r) r (Megaparsec.Token s)
noneOf cs = satisfy (\x -> notElem x cs)
{-# INLINE noneOf #-}

-- | @'chunk' chk@ only matches the chunk @chk@.
--
-- > divOrMod = chunk "div" <|> chunk "mod"
--
-- See also: 'tokens', 'Text.Megaparsec.Char.string',
-- 'Text.Megaparsec.Byte.string'.
chunk ::
  (MonadParsec e s m) =>
  -- | Chunk to match
  Megaparsec.Tokens s ->
  m r r (Megaparsec.Tokens s)
chunk s = tokens (==) s Cont2.@ s
{-# INLINE chunk #-}

-- | A synonym for 'label' in the form of an operator.
infix 0 <?>

(<?>) :: (MonadParsec e s m) => m r r' a -> String -> m r r' a
(<?>) = flip label
{-# INLINE (<?>) #-}

-- | Consume the rest of the input and return it as a chunk. This parser
-- never fails, but may return the empty chunk.
--
-- > takeRest = takeWhileP Nothing (const True)
takeRest :: (MonadParsec e s m) => m (Megaparsec.Tokens s -> r) r (Megaparsec.Tokens s)
takeRest = takeWhileP Nothing (const True)
{-# INLINE takeRest #-}

-- | Return 'True' when end of input has been reached.
--
-- > atEnd = option False (True <$ hidden eof)
atEnd :: (MonadParsec e s m) => m r r Bool
atEnd = (True <$ hidden eof) <|> pure False
{-# INLINE atEnd #-}

---------------------------------------------------------------------------
--
-- Building a MonadParsec
--
---------------------------------------------------------------------------

instance (Megaparsec.MonadParsec e s m) => MonadParsec e s (Indexed.IgnoreIndices m) where
  parseError = Indexed.IgnoreIndices . Megaparsec.parseError
  label s (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.label s a
  try (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.try a
  hidden (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.hidden a
  lookAhead (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.lookAhead (Megaparsec.try a)
  notFollowedBy (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.notFollowedBy a
  withRecovery f (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.withRecovery (Indexed.unIgnoreIndices . f) a
  observing (Indexed.IgnoreIndices a) = Indexed.IgnoreIndices $ Megaparsec.observing (Megaparsec.try a)
  eof = Indexed.IgnoreIndices Megaparsec.eof
  token f s _p = Indexed.IgnoreIndices $ Megaparsec.token f s
  tokens f s = Indexed.IgnoreIndices $ Megaparsec.tokens f s
  takeWhileP s f = Indexed.IgnoreIndices $ Megaparsec.takeWhileP s f
  takeWhile1P s f = Indexed.IgnoreIndices $ Megaparsec.takeWhile1P s f
  takeP s n = Indexed.IgnoreIndices $ Megaparsec.takeP s n

instance (MonadParsec e s m1, MonadParsec e s m2) => MonadParsec e s (m1 Indexed.:*: m2) where
  parseError e = parseError e Indexed.:*: parseError e
  label s (a Indexed.:*: b) = label s a Indexed.:*: label s b
  try (a Indexed.:*: b) = try a Indexed.:*: try b
  hidden (a Indexed.:*: b) = hidden a Indexed.:*: hidden b
  lookAhead (a Indexed.:*: b) = lookAhead a Indexed.:*: lookAhead b
  notFollowedBy (a Indexed.:*: b) = notFollowedBy a Indexed.:*: notFollowedBy b
  withRecovery f (a Indexed.:*: b) = withRecovery (Indexed.fst_star . f) a Indexed.:*: withRecovery (Indexed.snd_star . f) b
  observing (a Indexed.:*: b) = observing a Indexed.:*: observing b
  eof = eof Indexed.:*: eof
  token f s p = token f s p Indexed.:*: token f s p
  tokens f s = tokens f s Indexed.:*: tokens f s
  takeWhileP s f = takeWhileP s f Indexed.:*: takeWhileP s f
  takeWhile1P s f = takeWhile1P s f Indexed.:*: takeWhile1P s f
  takeP s n = takeP s n Indexed.:*: takeP s n
