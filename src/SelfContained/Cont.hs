{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module SelfContained.Cont where

import Data.Char (isDigit)
import SelfContained.Shared
import Prelude hiding (Applicative (..), Monad (..), (<$>))
import Prelude qualified

class Stacked m where
  shift_ :: (r -> m k r' k) -> m r r' ()

push :: (IxMonad m, Stacked m) => a -> m (a -> r) r ()
push a = shift_ \k -> return (k a)

pop_ :: (IxMonad m, Stacked m) => m r (a -> r) ()
pop_ = shift_ \k -> return (\_a -> k)

(@) :: (IxMonad m, Stacked m) => m r (a -> r') b -> a -> m r r' b
m @ a = shift_ (\k -> return (k a)) *> m

stack :: (IxMonad m, Stacked m) => (r -> r') -> m r r' ()
stack f = shift_ \k -> return (f k)

newtype Cont r r' a
  = Cont {runCont :: (a -> r) -> r'}

instance IxMonad Cont where
  return x = Cont \k -> k x
  (Cont c) >>= f = Cont \k ->
    c (\x -> runCont (f x) k)

instance Stacked Cont where
  shift_ f = shift \k -> f (k ())

shift :: ((a -> r) -> Cont k r' k) -> Cont r r' a
shift f = Cont \k -> runCont (f k) id

pop :: Cont r (a -> r) a
pop = shift \k -> return (\a -> k a)

instance
  (Stacked f, Stacked g) =>
  Stacked (f :*: g)
  where
  shift_ f = (shift_ (ifst . f) :*: shift_ (isnd . f))

instance (Prelude.Monad m) => Stacked (Fwd m) where
  shift_ _f = Fwd (Prelude.return ())

newtype ContT m r r' a
  = ContT {runContT :: (a -> m r) -> m r'}

instance IxMonad (ContT m) where
  return x = ContT \k -> k x
  (ContT c) >>= f = ContT \k ->
    c \x -> runContT (f x) k

lift :: (Prelude.Monad m) => m a -> ContT m r r a
lift act = ContT \k -> act Prelude.>>= k

newtype ContW w r r' a
  = ContW {runContW :: w (a -> r) -> r'}

shiftw :: (Comonad w) => ((a -> r) -> ContW w k r' k) -> ContW w r r' a
shiftw f = ContW \wk -> runContW (f (extract wk)) (const id `fmap` wk)

popw :: (Comonad w) => ContW w r (a -> r) a
popw = shiftw \k -> return (\a -> k a)

instance (Comonad w) => IxMonad (ContW w) where
  return x = ContW \wk -> extract wk x
  (ContW a) >>= f = ContW \wk -> a (extend k' wk)
    where
      k' wk x = runContW (f x) wk

instance (Comonad w) => Stacked (ContW w) where
  shift_ f = ContW \wk ->
    -- Notice how the continuation and its
    -- comonadic context are split, but both
    -- are passed to f.
    runContW (f (extract wk ())) (const id `fmap` wk)

yield :: (Comonad w) => (w r -> r) -> ContW w r r ()
yield eff = ContW \wk ->
  (eff ((\k -> k ()) `fmap` wk))

instance (ComTraced String w) => Descr (ContW w) where
  satisfy _ =
    popw >>= \c ->
      yield (trace [c]) *> return c

type D = Fwd Prs :*: ContW (Traced String)

type C m = (IxMonad m, Stacked m, Descr m)

lit :: (C m) => String -> m r r ()
lit [] = return ()
lit (c : cs) =
  satisfy (== c) @ c >>= \_ -> lit cs

char :: (C m) => m r (Char -> r) Char
char = satisfy (const True)

digit :: (C m) => m r (Int -> r) Int
digit = return (\c -> read [c]) <* stack (\k -> k . head . show) <*> satisfy isDigit

spec :: (C m) => m r (Int -> Char -> Char -> r) (Int, Char, Char)
spec = (,,) <$> digit <* lit "-th character after " <*> char <* lit " is " <*> char

-- | Ex:
-- >>> sprintf spec 5 'a' 'f'
-- "5-th character after a is f"
sprintf :: D String r a -> r
sprintf (_ :*: ContW pr) = pr (Traced (\s _ -> s))

-- | Ex:
-- >>> sscanf spec "5-th character after a is f"
-- Just (5, 'a', f')
sscanf :: D r r' a -> String -> Maybe a
sscanf (Fwd (Prs pa) :*: _) s = fst Prelude.<$> pa s
