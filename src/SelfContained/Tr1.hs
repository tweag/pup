{-# LANGUAGE ViewPatterns #-}

module SelfContained.Tr1 where

import Control.Category
import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Traced
import Prelude hiding (id, (.))

-- data (p :*: q) r r' = (:*:) {cfst :: p r r', csnd :: q r r'}

-- instance (Category p, Category q) => Category (p :*: q) where
--   id = id :*: id
--   (p1 :*: q1) . (p2 :*: q2) = (p1 . p2) :*: (q1 . q2)

-- type K9 = Cokleisli (Traced String) :*: Cokleisli (Store String)

-- -- Yes, I can't be bothered to import Profunctor.
-- class (Category p) => CProf p where
--   arr :: (a -> b) -> p a b

-- instance (CProf p, CProf q) => CProf (p :*: q) where
--   arr f = arr f :*: arr f

-- instance (Comonad w) => CProf (Cokleisli w) where
--   arr f = Cokleisli $ \wa -> f (extract wa)

-- class Stacked p where
--   shift_ :: (r -> p (k -> k) r') -> p r r'

-- push :: (Stacked p, CProf p) => a -> p (a -> r) r
-- push a = shift_ $ \k -> arr (\_ -> k a)

-- pop_ :: (Stacked p, CProf p) => p r (a -> r)
-- pop_ = shift_ $ \k -> arr (\_ _ -> k)

-- instance (Comonad w) => Stacked (Cokleisli w) where
--   shift_ f = Cokleisli $ \wk -> runCokleisli (f (extract wk)) (const id <$> wk)

-- instance (Stacked p, Stacked q) => Stacked (p :*: q) where
--   shift_ f = shift_ (cfst . f) :*: shift_ (csnd . f)

-- class Switching p where
--   switch :: (a -> p r r') -> p r (a -> r')

-- instance (Switching p, Switching q) => Switching (p :*: q) where
--   switch f = switch (cfst . f) :*: switch (csnd . f)

-- instance (Comonad w) => Switching (Cokleisli w) where
--   switch f = Cokleisli $ \wk a -> runCokleisli (f a) wk

-- class Descr p where
--   satisfy :: (Char -> Bool) -> p (Char -> r) (Char -> r)

-- instance (Descr p, Descr q) => Descr (p :*: q) where
--   satisfy f = satisfy f :*: satisfy f

-- instance Descr (Cokleisli (Traced String)) where
--   satisfy _f = Cokleisli $ \(runTraced -> k) c -> k [c] c

-- instance Descr (Cokleisli (Store String)) where
--   satisfy f = Cokleisli $ \(runStore -> (k, s)) _c -> case s of
--     [] -> error "end of input"
--     (c : cs)
--       | f c -> k cs c
--       | otherwise -> error $ "character '" ++ [c] ++ "' unexpected"

-- char :: (Descr p, Stacked p, CProf p) => p (Char -> r) (Char -> r)
-- char = satisfy (const True)

-- digit :: (Descr p, Stacked p, CProf p) => p (Int -> r) (Int -> r)
-- digit = arr (\k -> k . head . show) . char . arr (\k -> k . read . (: []))

-- lit :: (Descr p, Stacked p, CProf p) => String -> p r r
-- lit [] = arr id
-- lit (c : cs) =
--   push c . satisfy (== c) . pop_ . lit cs

-- pretty :: K9 (a -> String) (b -> String) -> b -> String
-- pretty (Cokleisli pr :*: _) b = pr (traced (\s _ -> s)) b

-- parse :: K9 (a -> a) (b -> a) -> String -> a
-- parse (_ :*: Cokleisli pa) s = pa (store (\_ a -> a) s) (error "I so swear")

-- spec = digit . lit "−th character after " . char . lit " is " . char

newtype Tr r r' = Tr {unTr :: r -> r'}

newtype Flp p r r' = Flp {unFlp :: forall k. p (r -> k) (r' -> k)}

switch :: (a -> Flp Tr r r') -> Flp Tr (a -> r) r'
switch f = Flp $ Tr $ \k k' -> k (\a -> unTr (unFlp (f a)) id k')

switchw :: (Comonad w) => (a -> Flp (Cokleisli w) r r') -> Flp (Cokleisli w) (a -> r) r'
switchw f = Flp $ Cokleisli $ \k k' -> extract k (\a -> runCokleisli (unFlp (f a)) (const id <$> k) k')

newtype Pr r r' o o' = Pr {unPr :: (o' -> r) -> (o -> r')}

switchd :: (a -> Pr r r' o o') -> Pr r r' o (a -> o')
switchd f = Pr $ \k ok -> _

-- class Switching p where
--   switch :: (a -> p r r') -> p r (a -> r')

-- instance Switching Tr where
--   switch f = Tr $ \k s a -> unTr (f a) k s

-- -- instance Switching p => Switching (Flp p) where
-- --   switch f = Flp $ switch (\a -> unFlp (f a))

-- -- instance Switching (Flp Tr) where
-- --   switch f = Flp $ Tr $ \k s k' -> _

-- switch' :: (a -> Flp Tr r r') -> Flp Tr (a -> r) r'
-- switch' f = Flp $ Tr $ \k s k' -> _
--   where
--     f' a = unTr $ unFlp $ f a
--     f'' a k s k' = f' a k s k' a
