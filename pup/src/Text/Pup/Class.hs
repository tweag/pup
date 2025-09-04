{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module declares common high-level format-descriptor classes which are
-- largely independent from the particular backend. Classes in this module are
-- abstract over the type of token that you parse or print. Classes specialised
-- to certain tokens can be found in "Text.Pup.Class.Char".
--
-- The relevant class are re-exported by the modules which define backends. So
-- unless you are defining a new backend, you shouldn't have to import this
-- module.
--
-- Many docstrings in this module are adapted from Megparsec (see
-- "Text.Megaparsec") and Prettyprinter (see "Prettyprinter").
module Text.Pup.Class
  ( -- * Basic token format descriptors
    Tokens (..),
    (<?>),
    chunk,
    anySingle,
    anySingleBut,
    oneOf,
    noneOf,
    takeRest,
    atEnd,

    -- * Breaks
    Breaks (..),

    -- * Wadler-Leijen-style grouping
    WadlerLeijen (..),

    -- * Look-ahead descriptors
    LookAhead (..),

    -- * Parse errors
    ParseErrors (..),

    -- * Deriving-via
    Trivial (..),
  )
where

import Control.Additive
import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Data.Functor

-- | This class declares the basic format descriptors for tokens (what the
-- theory of grammars call terminals).
class (Eq tok, Eq chunk) => Tokens tok chunk m | m -> tok chunk where
  -- | This parser only succeeds at the end of input.
  eof :: m r r ()

  -- | As a parser, @'single' t@ only matches the single token @t@. As a
  -- printer, @'single' t@  prints the token @t@.
  --
  -- > semicolon = single ';'
  --
  -- See also: 'token', 'anySingle', 'Text.Pup.Class.Char'.
  single :: tok -> m r r tok

  -- | As a parser @'satisfy' p@ succeeds for any token for which the supplied
  -- predicate pf@ returns 'True'. As a printer @'satisfy' p@ prints its input
  -- token, provided predicate @p@ returns 'True', and fails otherwise.
  --
  -- > digitChar = satisfy isDigit <?> "digit"
  -- > oneOf cs  = satisfy (`elem` cs)
  --
  -- See also: 'anySingle', 'anySingleBut', 'oneOf', 'noneOf'.
  satisfy :: (tok -> Bool) -> m (tok -> r) r tok

  -- | As a parser @'tokens' test chk@ takes chunk @chk'@ of input of length @length
  -- chk@ and returns @chk'@ if the equality test @test chk chk'@ succeeds, and
  -- fails otherwise. As a printer, @'tokens' test chk@ prints its input chunk
  -- @chk'@ provided that @length chk' = length chk@ and the equality test @test
  -- chk chk'@ succeeds, and fails otherwise.
  --
  -- This can be used for example to write 'chunk':
  --
  -- > chunk = tokens (==)
  tokens ::
    -- | Predicate to check equality of chunks
    (chunk -> chunk -> Bool) ->
    -- | Chunk of input to match against
    chunk ->
    m (chunk -> r) r chunk

  -- | As a parser, parses /zero/ or more tokens for which the supplied
  -- predicate holds. As a printer, prints its input chunk of tokens, provided
  -- that each token in the chunk satisfies the predicate.
  --
  -- This is an optimised variant of parsers built with
  -- 'Control.Monad.Indexed.Cont2.many':
  --
  -- > takeWhileP (Just "foo") f = many (satisfy f <?> "foo")
  -- > takeWhileP Nothing      f = many (satisfy f)
  --
  -- The parser side never fails, although it may parse the empty chunk.
  takeWhileP ::
    -- | (Optional) name for a single token, can be used in error messages
    Maybe String ->
    -- | Predicate to use to test tokens
    (tok -> Bool) ->
    m (chunk -> r) r chunk

  -- | Similar to 'takeWhileP', but fails if it can't parse or print at least one
  -- token.
  --
  -- This is an optimised variant of parsers built with
  -- 'Control.Monad.Indexed.Cont2.some':
  --
  -- > takeWhile1P (Just "foo") f = some (satisfy f <?> "foo")
  -- > takeWhile1P Nothing      f = some (satisfy f)
  takeWhile1P ::
    -- | (Optional) name for a single token, can be used in error messages
    Maybe String ->
    -- | Predicate to use to test tokens
    (tok -> Bool) ->
    m (chunk -> r) r chunk

  -- | As a parser @takeP lbl n@ returns the next @n@ tokens from the input (it
  -- fails if there are not enough tokens in the input). As a printer, it
  -- prints its input chunk of tokens, provided that the chunk has exactly @n@
  -- tokens.
  takeP ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | How many tokens to extract
    Int ->
    m (chunk -> r) r chunk

  -- | @'label' lbl p@ gives a name to the next token expected by @p@. This is
  -- only really useful for the parser side, where it can be used to report
  -- error messages of the form “unexpected ..., expected lbl”.
  label :: String -> m r r' a -> m r r' a

  -- | @'hidden' p@ silences error messages of the form “unexpected ...,
  -- expected ...” in @p@ (only relevant on the parser side).
  hidden :: m r r' a -> m r r' a
  hidden = label ""

-- | A synonym for 'label' in the form of an operator.
(<?>) :: (Tokens chunk tok m) => m r r' a -> String -> m r r' a
(<?>) = flip label
{-# INLINE (<?>) #-}

infix 0 <?>

-- | Parses/prints a single token.
--
-- > anySingle = satisfy (const True)
--
-- See also: 'satisfy', 'anySingleBut'.
anySingle :: (Tokens tok chunk m) => m (tok -> r) r (tok)
anySingle = satisfy (const True)
{-# INLINE anySingle #-}

-- | Parse/print any token but the given one. It's a good idea to attach a
-- 'label' to this format descriptor.
--
-- > anySingleBut t = satisfy (/= t)
--
-- See also: 'single', 'anySingle', 'satisfy'.
anySingleBut ::
  (Tokens tok chunk m) =>
  -- | Token we should not match
  tok ->
  m (tok -> r) r tok
anySingleBut t = satisfy (/= t)
{-# INLINE anySingleBut #-}

-- | @'oneOf' ts@ parses/prints any token from @ts@. Note that, as a parser,
-- @'oneOf' ts@ can't automatically generate “expected” tokens in error
-- messages, so you should usually label it manually with 'label' or ('<?>').
--
-- > oneOf cs = satisfy (`elem` cs)
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"
oneOf ::
  (Foldable f, Tokens tok chunk m) =>
  -- | Collection of matching tokens
  f tok ->
  m (tok -> r) r tok
oneOf cs = satisfy (\x -> elem x cs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @'noneOf' ts@ succeeds and parses/prints the
-- current token if the current token /isn't/ in @ts@.
-- Note that, as a parser, @'oneOf' ts@ can't automatically generate “expected”
-- tokens in error messages, so you should usually label it manually with
-- 'label' or ('<?>').
--
-- > noneOf cs = satisfy (`notElem` cs)
--
-- See also: 'satisfy'.
noneOf ::
  (Foldable f, Tokens tok chunk m) =>
  -- | Collection of taken we should not match
  f tok ->
  m (tok -> r) r tok
noneOf cs = satisfy (\x -> notElem x cs)
{-# INLINE noneOf #-}

-- | @'chunk' chk@ only matches the chunk @chk@.
--
-- > divOrMod = chunk "div" <|> chunk "mod"
--
-- When @tok@ is 'Char', it's customary for format descriptors to implement
-- the 'IsString' class with @'fromString' = 'chunk'@.
--
-- See also: 'tokens', 'Text.Pup.Class.Char.string'.
chunk ::
  (Indexed.Applicative m, Cont2.Stacked m, Tokens tok chunk m) =>
  -- | Chunk to match
  chunk ->
  m r r ()
chunk s = void $ tokens (==) s Cont2.@ s
{-# INLINE chunk #-}

-- | Consume the rest of the input and return it as a chunk. This parser
-- never fails, but may return the empty chunk.
--
-- > takeRest = takeWhileP Nothing (const True)
takeRest :: (Tokens tok chunk m) => m (chunk -> r) r chunk
takeRest = takeWhileP Nothing (const True)
{-# INLINE takeRest #-}

-- | Return 'True' when end of input has been reached.
--
-- > atEnd = option False (True <$ hidden eof)
atEnd :: (Indexed.Alternative m, Tokens tok chunk m) => m r r Bool
atEnd = (True <$ hidden eof) <|> Indexed.pure False
{-# INLINE atEnd #-}

instance (Tokens tok chunk m1, Tokens tok chunk m2) => Tokens tok chunk (m1 Indexed.:*: m2) where
  label s (a Indexed.:*: b) = label s a Indexed.:*: label s b
  hidden (a Indexed.:*: b) = hidden a Indexed.:*: hidden b
  single t = single t Indexed.:*: single t
  satisfy f = satisfy f Indexed.:*: satisfy f
  eof = eof Indexed.:*: eof
  tokens f s = tokens f s Indexed.:*: tokens f s
  takeWhileP s f = takeWhileP s f Indexed.:*: takeWhileP s f
  takeWhile1P s f = takeWhile1P s f Indexed.:*: takeWhile1P s f
  takeP s n = takeP s n Indexed.:*: takeP s n

-- | The typeclass @'Breaks' m@ implements format descriptors which represents
-- breaks. These are typically spaces and newlines. Because breaks can be many
-- spaces and/or newlines, the printer side must make decisions about how many
-- of which should be printed for a single break. Which is why these format
-- descriptors aren't merely derived from 'satisfy'.
class Breaks m where
  -- | This parses 0 or more spaces/newline characters. As a printer a typical
  -- choice is to print no space at all.
  space :: m r r ()

  -- | This parses 1 or more spaces/newline characters. As a printer a typical
  -- choice is to print a soft newline (see also 'group').
  space1 :: m r r ()

  -- | This parses/prints a newline character.
  hardline :: m r r ()

instance (Breaks m1, Breaks m2) => Breaks (m1 Indexed.:*: m2) where
  space = space Indexed.:*: space
  space1 = space1 Indexed.:*: space1
  hardline = hardline Indexed.:*: hardline

-- | A class abstracting over the layout strategy of Wadler-Leijen-type pretty
-- printers. All these format descriptors are no-ops on the parser side.
class (Breaks m) => WadlerLeijen m where
  -- | @'group' x@ tries laying out @x@ into a single line by interpreting the
  -- contained line breaks (/e.g./ space1') as spaces; if this does not fit the
  -- page, or when a hardline (see 'hardline') within @x@ prevents it from being
  -- flattened, @x@ is laid out without any changes.
  group :: m r r' a -> m r r' a

  -- | @'nest' i x@ lays out the document @x@ with the current nesting level
  -- (indentation of the following lines) increased by @i@. Negative values are
  -- allowed, and decrease the nesting level accordingly.
  --
  -- >>> nest 4 ("lorem" <> space1 <> "ipsum" <> "dolor") <> line <> "sit" <> line "amet"
  -- lorem
  --     ipsum
  --     dolor
  -- sit
  -- amet
  nest :: Int -> m r r' a -> m r r' a

instance (WadlerLeijen m1, WadlerLeijen m2) => WadlerLeijen (m1 Indexed.:*: m2) where
  group (a Indexed.:*: b) = group a Indexed.:*: group b
  nest n (a Indexed.:*: b) = nest n a Indexed.:*: nest n b

instance (Breaks m) => WadlerLeijen (Trivial m) where
  group = id
  nest _n = id

-- | This class declares look-ahead combinators. These allow parsers to react to
-- the upcomming of the input without consuming it. They are ignored by printers.
class LookAhead m where
  -- | If @p@ in @'lookAhead' p@ succeeds (either consuming input or not) the
  -- whole parser behaves like @p@ succeeded without consuming anything (parser
  -- state is not updated as well). If @p@ fails @'lookAhead' p@ fails without
  -- consuming input.
  lookAhead :: m (a -> r) r a -> m (a -> r) r a

  -- | @'notFollowedBy' p@ only succeeds when the parser @p@ fails. This
  -- parser /never consumes/ any input and /never modifies/ parser state. It
  -- can be used to implement the “longest match” rule.
  notFollowedBy :: m r r a -> m r r ()

instance (LookAhead m1, LookAhead m2) => LookAhead (m1 Indexed.:*: m2) where
  lookAhead (a Indexed.:*: b) = lookAhead a Indexed.:*: lookAhead b
  notFollowedBy (a Indexed.:*: b) = notFollowedBy a Indexed.:*: notFollowedBy b

instance (Indexed.Applicative m, Cont2.Shifty m) => LookAhead (Trivial m) where
  lookAhead _ = Cont2.pop
  notFollowedBy _ = Indexed.pure ()

-- | A class for error handling in parsers.
class ParseErrors e m where
  -- | Stop parsing and report the 'e' as a parsing error. No-op as a printer.
  parseError :: e -> m r r' a

  -- | @'withRecovery' r p@ allows us to continue parsing even if the parser
  -- @p@ fails. In this case @r@ is called with the actual 'ParseError' as
  -- its argument. Typical usage is to return a value signifying failure to
  -- parse this particular object and to consume some part of the input up
  -- to the point where the next object starts.
  --
  -- Note that if @r@ fails, the original error message is reported as if
  -- without 'withRecovery'. In no way recovering parser @r@ can influence
  -- error messages.
  --
  -- No-op as a printer.
  withRecovery ::
    -- | How to recover from failure
    (e -> m r r' a) ->
    -- | Original parser
    m r r' a ->
    -- | Parser that can recover from failures
    m r r' a

  -- | @'observing' p@ allows us to "observe" failure of the @p@ parser,
  -- should it happen, without actually ending parsing but instead getting
  -- the 'ParseError' in 'Left', and no input is consumed. On success parsed value is returned in
  -- 'Right' as usual.
  --
  -- As a printer @'observing' p@ is a no-op on 'Left' and behaves like @p@ on
  -- 'Right'.
  observing ::
    -- | The parser to run
    m (b -> r) r a ->
    m ((Either e b) -> r) r (Either e a)

instance (ParseErrors e m1, ParseErrors e m2) => ParseErrors e (m1 Indexed.:*: m2) where
  parseError e = parseError e Indexed.:*: parseError e
  withRecovery f (a Indexed.:*: b) = withRecovery (Indexed.fst_star . f) a Indexed.:*: withRecovery (Indexed.snd_star . f) b
  observing (a Indexed.:*: b) = observing a Indexed.:*: observing b

-- | A deriving-via constructor to derive instances for the classes in this
-- module which can be implemented as no-ops.
newtype Trivial m r r' a = MkTrivial (m r r' a)
  deriving newtype (Functor, Applicative, Monad, Additive, Indexed.Applicative, Indexed.Monad, Cont2.Stacked, Cont2.Shifty, Breaks)
