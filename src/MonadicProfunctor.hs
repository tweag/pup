{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module MonadicProfunctor where

import Control.Applicative
import Control.Monad
import Data.Char qualified as Char
import Data.Coerce

-- Option, if defining our own: make `Functor` a superclass, and only define
-- `lmap` as a method
-- class (forall u. Functor (p u)) => Profunctor p where
class Profunctor p where
  rmap :: (b -> c) -> p a b -> p a c
  lmap :: (a -> b) -> p b c -> p a c

upon :: (Profunctor p) => p b c -> (a -> b) -> p a c
upon = flip lmap

data (:*:) p q u v = (:*:) (p u v) (q u v)
  deriving (Functor)

pfst :: (p :*: q) u v -> p u v
pfst (f :*: _) = f

psnd :: (p :*: q) u v -> q u v
psnd (_ :*: g) = g

instance (Profunctor p, Profunctor q) => Profunctor (p:*:q) where
  rmap f (p :*: q) = (rmap f p) :*: (rmap f q)
  lmap g (p :*: q) = (lmap g p) :*: (lmap g q)

instance (Applicative (p u), Applicative (q u)) => Applicative ((p :*: q) u) where
  pure a = pure a :*: pure a
  (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

instance (Monad (p u), Monad (q u)) => Monad ((p :*: q) u) where
  (a :*: b) >>= k = (a >>= pfst . k) :*: (b >>= psnd . k)

instance (Alternative (p u), Alternative (q u)) => Alternative ((p :*: q) u) where
  empty = empty :*: empty
  (a :*: b) <|> (a' :*: b') = (a <|> a') :*: (b <|> b')

instance (MonadPlus (p u), MonadPlus (q u)) => MonadPlus ((p :*: q) u) where

newtype Fwd m u v = Fwd { unFwd :: m v }
  deriving (Functor)

newtype Bwd m u v = Bwd { unBwd :: u -> m v}
  deriving (Functor)

instance (Functor m) => Profunctor (Fwd m) where
  rmap f (Fwd a) = Fwd $ f <$> a
  lmap _ (Fwd a) = Fwd a

instance (Functor m) => Profunctor (Bwd m) where
  rmap f (Bwd a) = Bwd $ (fmap f) <$> a
  lmap g (Bwd a) = Bwd (a . g)

instance (Applicative m) => Applicative (Fwd m u) where
  pure a = Fwd $ pure a
  (Fwd f) <*> (Fwd a) = Fwd $ f <*> a

instance (Applicative m) => Applicative (Bwd m u) where
  pure a = Bwd $ \_ -> pure a
  (Bwd f) <*> (Bwd a) = Bwd $ \u -> f u <*> a u

instance (Monad m) => Monad (Fwd m u) where
  (Fwd a) >>= k = Fwd $ a >>= \x -> unFwd (k x)

instance (Monad m) => Monad (Bwd m u) where
  (Bwd a) >>= k = Bwd $ \u -> a u >>= \x -> unBwd (k x) u

instance (Alternative m) => Alternative (Fwd m u) where
  empty = Fwd $ empty
  (Fwd a) <|> (Fwd b) = Fwd $ a <|> b

instance (Alternative m) => Alternative (Bwd m u) where
  empty = Bwd $ \ _ -> empty
  (Bwd a) <|> (Bwd b) = Bwd $ \s -> a s <|> b s

instance (MonadPlus m) => MonadPlus (Fwd m u)

instance (MonadPlus m) => MonadPlus (Bwd m u)

-- StateT Maybe, really, but it was quicker to write this way without access to
-- deps.
newtype MayState s a = MayState { runState :: s -> Maybe (a, s) }
  deriving stock (Functor)

instance Applicative (MayState s) where
  pure a = MayState $ \s -> Just (a, s)
  (<*>) = ap

instance Monad (MayState s) where
  (MayState a) >>= k = MayState $ \s -> case a s of
    Just (x, s')-> runState (k x) s'
    Nothing -> Nothing

instance Alternative (MayState s) where
  empty = MayState $ \_-> Nothing
  (MayState a) <|> (MayState b) = MayState $ \s ->
    a s <|> b s

instance MonadPlus (MayState s)

-- WriterT Maybe, really, but it was quicker to write this way without access to
-- deps.
newtype MayWrite w a = MayWrite { runMayWrite :: Maybe (a, w) }
  deriving stock (Functor)

instance Monoid w => Applicative (MayWrite w) where
  pure a = MayWrite $ Just (a, mempty)
  (MayWrite (Just (f, wf))) <*> (MayWrite (Just (x, wx))) = MayWrite $ Just (f x, wf <> wx)
  _ <*> _ = MayWrite Nothing

instance Monoid w => Monad (MayWrite w) where
  (MayWrite (Just (a, w))) >>= k = MayWrite $ case runMayWrite (k a) of
    Just (b, wb)-> Just (b, w <> wb)
    Nothing -> Nothing
  MayWrite Nothing >>= _ = MayWrite Nothing

instance Monoid w => Alternative (MayWrite w) where
  empty = MayWrite $ Nothing
  (MayWrite a) <|> (MayWrite b) = MayWrite $ a <|> b

instance Monoid w => MonadPlus (MayWrite w)

------------------------------------------------------------------

type Biparser = Fwd (MayState String) :*: Bwd (MayWrite String)

-- Possibly derivable from the MonadPlus instance of `MayWrite`
mupon :: Biparser u v -> (w -> Maybe u) -> Biparser w v
mupon (Fwd prs :*: Bwd prnt) f = Fwd prs :*: Bwd prnt'
  where
    prnt' w = case f w of
      Just u -> prnt u
      Nothing -> MayWrite Nothing

mkBP :: (String -> Maybe (v, String)) -> (u -> Maybe (v, String)) -> Biparser u v
mkBP prs prnt = (coerce prs) :*: (coerce prnt)

parse :: Biparser u v -> String -> Maybe (v, String)
parse (prs :*: _) = coerce prs

print' :: Biparser u v -> u -> Maybe (v, String)
print' (_ :*: prnt) = coerce prnt

print :: Biparser u v -> u -> Maybe String
print bp = (fmap snd) . print' bp

anyChar :: Biparser Char Char
anyChar = mkBP (\cases { (c:s) -> Just (c, s); [] -> Nothing }) (\ c -> (Just (c, [c])))

anyOneChar :: Biparser String String
anyOneChar = do
  c <- anyChar `upon` head
  return [c]

char :: Char -> Biparser u ()
char c = do
  c' <- anyChar `upon` (const c)
  guard $ c == c'

space :: Biparser u ()
space = char ' '

string :: String -> Biparser u ()
string s = forM_ s char

digit :: Biparser Int Int
digit = do
  c <- anyChar `upon` (head . show)
  guard $ Char.isDigit c
  return $ read [c]

bool :: Biparser Bool Bool
bool = do
  ((char 't' >> return True) `mupon` isTrue)
  <|> ((char 'f' >> return False) `mupon` isFalse)
  where
    isTrue True = Just ()
    isTrue False = Nothing

    isFalse True = Nothing
    isFalse False = Just ()

test :: Biparser (Either Int String) (Either Int String)
test =
  ((char 'L' >> space >> digit >>= return . Left) `mupon` unL)
  <|> ((char 'R' >> space >> anyOneChar >>= return . Right) `mupon` unR)
  where
    unL (Left i) = Just i
    unL (Right _) = Nothing

    unR (Left _) = Nothing
    unR (Right c) = Just c

data Foo
  = Bar Int String
  | Baz String Bool Int
  deriving (Show)

testFoo :: Biparser Foo Foo
testFoo =
  ((pure Bar <* string "Bar " <*> (digit `upon` fst) <* space <*> (anyOneChar `upon` snd)) `mupon` unBar)
  <|> ((pure Baz <* string "Baz " <*> (anyOneChar `upon` (\(c,_,_) -> c)) <* space <*> (bool `upon` (\(_,b,_) -> b)) <* space <*> (digit `upon` (\(_,_,i) -> i))) `mupon` unBaz)
  where
    unBar (Bar i c) = Just (i, c)
    unBar _ = Nothing

    unBaz (Baz c b i) = Just (c, b, i)
    unBaz _ = Nothing

-- Type of printers according to Xia &al:
-- u -> Maybe (v, String)
--
-- A Danvified version would look like:
-- (Maybe (v, String) -> r) -> u -> r
