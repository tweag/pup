{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module IndexedMonad where

import Data.Char qualified as Char
import Data.Coerce
import Prelude hiding ((*>))

data Print rf r r' a = Print {runPrint :: rf -> (a -> String -> rf -> r') -> r}
  deriving (Functor)

pprint :: Print (Maybe String) r (Maybe String) a -> r
pprint prnt = runPrint prnt Nothing (\_ s _ -> Just s)

mapk :: (r -> r') -> Print rf r r'' a -> Print rf r' r'' a
mapk f (Print prnt) = Print $ \fl k -> f (prnt fl k)

mapkm :: (rf -> r -> r') -> Print rf r r'' a -> Print rf r' r'' a
mapkm f (Print prnt) = Print $ \fl k -> f fl (prnt fl k)

(@) :: Print rf (a -> r) r' b -> a -> Print rf r r' b
prnt @ a = mapk (\f -> f a) prnt

ppure :: a -> Print rf r r a
ppure x = Print $ \fl k -> k x "" fl

(<*>) :: Print rf r r' (a -> b) -> Print rf r' r'' a -> Print rf r r'' b
ff <*> aa = ff IndexedMonad.>>= \f -> aa IndexedMonad.>>= \a -> ppure (f a)

(*>) :: Print rf r r' a -> Print rf r' r'' b -> Print rf r r'' b
aa *> bb = (fmap (\_ -> id) aa) IndexedMonad.<*> bb

(>>=) :: Print rf r r' a -> (a -> Print rf r' r'' b) -> Print rf r r'' b
(Print a) >>= kp = Print $ \fl k -> a fl $ \x sx flx ->
  runPrint (kp x) flx $ \y sy fly -> k y (sx ++ sy) fly

pempty :: Print r r r' a
pempty = Print $ \fl _k -> fl

(<|>) :: Print r r r' a -> Print r r r' a -> Print r r r' a
(Print lft) <|> (Print rgt) = Print $ \fl k ->
  lft (rgt fl k) k

---------------------------------------------------------

pint :: Print rf (Int -> r) r Int
pint = Print $ \fl k i -> k i (show i) fl

pstring :: Print rf (String -> r) r String
pstring = Print $ \fl k s -> k s s fl

pbool :: Print rf (Bool -> r) r Bool
pbool = Print $ \fl k b -> k b (show b) fl

pchar :: Print rf (Char -> r) r ()
pchar = Print $ \fl k c -> k () [c] fl

space :: Print rf r r ()
space = pchar @ ' '

----------------------------------------------------------

p2int :: Print rf (Int -> Int -> r) r ()
p2int = pint IndexedMonad.*> pint IndexedMonad.*> ppure ()

p2int' :: Print rf (Int -> Int -> r) r ()
p2int' = pint IndexedMonad.>>= \_ -> pint IndexedMonad.>>= \_ -> ppure ()

data Foo
  = Bar Int String
  | Baz String Bool Int
  deriving (Show)

pfoo :: Print (Foo -> r) (Foo -> r) r ()
pfoo =
  (unBar $ pstring @ "Bar " *> pint *> space *> pstring *> ppure ())
    <|> (unBaz $ pstring @ "Baz " *> pstring *> space *> pbool *> space *> pint *> ppure ())
  where
    unBar :: Print (Foo -> r) (Int -> String -> r) r' a -> Print (Foo -> r) (Foo -> r) r' a
    unBar = mapkm (\cases _fl k (Bar i s) -> k i s; fl _k x -> fl x)

    unBaz :: Print (Foo -> r) (String -> Bool -> Int -> r) r' a -> Print (Foo -> r) (Foo -> r) r' a
    unBaz = mapkm (\cases _fl k (Baz s b i) -> k s b i; fl _k x -> fl x)
