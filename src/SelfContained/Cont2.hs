{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module SelfContained.Cont2 where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Data.Char (isAlphaNum, isAscii, isLetter)
import SelfContained.Shared
import Prelude hiding (Applicative (..), Monad (..), (<$>))
import Prelude qualified

class (Stacked m, IxMonad m, forall r r' a. Monoid (m r r' a)) => Descr m where
  satisfy :: (Char -> Bool) -> m r (Char -> r) Char

instance (Descr f, Descr g) => Descr (f :*: g) where
  satisfy p = satisfy p :*: satisfy p

instance Descr (Fwd Prs) where
  satisfy p = Fwd (Prs go)
    where
      go (c : s) | p c = Just (c, s)
      go _ = Nothing

newtype Cont2W w r r' a = Cont2W
  {runCont2W :: w (a -> r -> r) -> r' -> r'}

instance (Comonad w) => IxMonad (Cont2W w) where
  return x = Cont2W \wk -> extract wk x
  Cont2W a >>= f = Cont2W \wk -> a (extend k' wk)
    where
      k' wk x = runCont2W (f x) wk

instance (Comonad w) => Monoid (Cont2W w r r' a) where
  mempty = Cont2W \_ fl -> fl

instance (Comonad w) => Semigroup (Cont2W w r r' a) where
  (Cont2W a) <> (Cont2W b) = Cont2W \wk fl ->
    a wk (b wk fl)

shiftw :: (Comonad w) => ((a -> r -> r) -> r' -> Cont2W w k r' k) -> Cont2W w r r' a
shiftw f = Cont2W \wk k' -> runCont2W (f (extract wk) k') (const (\k _ -> k) Prelude.<$> wk) k'

pop :: (Comonad w) => Cont2W w r (a -> r) a
pop = shiftw \k k' -> return (\a -> k a (k' a))

instance (ComTraced String w) => Descr (Cont2W w) where
  satisfy _ =
    pop >>= \c ->
      yield (trace [c]) *> return c

yield :: (Comonad w) => (w r -> r) -> Cont2W w r r ()
yield eff = Cont2W \wk k' ->
  eff ((\k -> k () k') Prelude.<$> wk)

instance (MonadPlus m) => Monoid (Fwd m r r' a) where
  mempty = Fwd empty

instance (MonadPlus m) => Semigroup (Fwd m r r' a) where
  (Fwd a) <> (Fwd b) = Fwd (a <|> b)

instance MonadPlus Prs

instance Alternative Prs where
  empty = Prs \_ -> empty
  (Prs pa1) <|> (Prs pa2) = Prs \s -> pa1 s <|> pa2 s

instance
  (Monoid (f r r' a), Monoid (g r r' a)) =>
  Monoid ((f :*: g) r r' a)
  where
  mempty = mempty <> mempty

instance
  (Monoid (f r r' a), Monoid (g r r' a)) =>
  Semigroup ((f :*: g) r r' a)
  where
  ~(fl :*: fr) <> ~(gl :*: gr) =
    (fl <> gl) :*: (fr <> gr)

class (IxMonad m) => Stacked m where
  shift_ ::
    ((r -> r) -> r' -> m k r' k) ->
    m r r' ()

instance (Comonad w) => Stacked (Cont2W w) where
  shift_ f = Cont2W \wk k' ->
    runCont2W
      (f (extract wk ()) k')
      ((\_k -> \x _ -> x) Prelude.<$> wk)
      k'

push :: (Stacked m) => a -> m (a -> r) r ()
push x = shift_ \k k' -> return (k (\_ -> k') x)

pop_ :: (Stacked m) => m r (a -> r) ()
pop_ = shift_ \k k' -> return (\a -> k (k' a))

stack ::
  (Stacked m) =>
  (r' -> r -> r') ->
  (r' -> r) ->
  m r r' ()
stack f u = shift_ \k k' -> return (f k' (k (u k')))

instance (Prelude.Monad m) => Stacked (Fwd m) where
  shift_ _f = Fwd (Prelude.return ())

instance (Stacked f, Stacked g) => Stacked (f :*: g) where
  shift_ f = (shift_ (\k k' -> ifst (f k k'))) :*: (shift_ (\k k' -> isnd (f k k')))

consL ::
  (Stacked m) =>
  m
    (a -> [a] -> r)
    ([a] -> r)
    (a -> [a] -> [a])
consL = stack uncons unroll *> return (:)
  where
    uncons k' k (x : xs) = k x xs
    uncons k' k [] = k' []

    unroll k' x xs = k' (x : xs)

data Prism' s a = Prism' {review :: a -> s, preview :: s -> Maybe a}

prismL ::
  (Stacked m) =>
  Prism' s a ->
  m (a -> r) (s -> r) (a -> s)
prismL l = stack rev u *> return (review l)
  where
    rev k' k t = case preview l t of
      Nothing -> k' t
      Just x -> k x

    u k' a = k' (review l a)

skip :: (Descr m) => m r r () -> m r r ()
skip p = (p <* skip p) <> return ()

some :: (Descr m) => (forall r. m r (a -> r) a) -> m r' ([a] -> r') [a]
some p = consL <*> p <*> many p

many :: (Descr m) => (forall r. m r (a -> r) a) -> m r' ([a] -> r') [a]
many p = some p <> (pop_ *> return [])

lit :: (Descr m) => String -> m r r ()
lit [] = return ()
lit (c : cs) =
  push c *> satisfy (== c) >>= \_ -> lit cs

letter = satisfy (\c -> isLetter c && isAscii c)

alphaNum = satisfy (\c -> isAlphaNum c && isAscii c)

parens :: D2 r r' a -> D2 r r' a
parens p = lit "(" *> p <* lit ")"

sepSpace = lit " " *> skip (lit " ")

idnt = consL <*> letter <*> many alphaNum

type Idnt = String

data Term = Var Idnt | Abs Idnt Term | App Term Term
  deriving (Show)

type D2 = Fwd Prs :*: Cont2W (Traced String)

term :: D2 r (Term -> r) Term
term =
  varL
    <*> idnt
      <> absL
    <* lit "λ"
    <*> idnt
    <* lit "."
    <*> term
      <> parens (appL <*> term <* sepSpace <*> term)

varL :: (Descr m) => m (Idnt -> r) (Term -> r) (Idnt -> Term)
varL = prismL _Var
  where
    _Var = Prism' {review = Var, preview = \case Var x -> Just x; _ -> Nothing}

absL :: (Descr m) => m (Idnt -> Term -> r) (Term -> r) (Idnt -> Term -> Term)
absL = stack (\k' k -> \case Abs x u -> k x u; t -> k' t) (\k x u -> k (Abs x u)) *> return Abs

appL :: (Descr m) => m (Term -> Term -> r) (Term -> r) (Term -> Term -> Term)
appL = stack (\k' k -> \case App u v -> k u v; t -> k' t) (\k u v -> k (App u v)) *> return App

-- |
-- >>> parse term "λx.(x x)"
-- Just (Abs "x" (App (Var "x") (Var "x")))
parse :: D2 r r' b -> String -> Maybe b
parse (Fwd (Prs pa) :*: _) s = fst Prelude.<$> pa s

-- |
-- >>> pretty term (parse term "!$\color{grayred}\lambda$!x.(x x)")
-- Just "!$\color{grayred}\lambda$!x.(x x)"
pretty :: D2 (Maybe String) (a -> Maybe String) b -> a -> Maybe String
pretty (_ :*: Cont2W pr) = pr (Traced (\s _ _ -> Just s)) (\_ -> Nothing)
