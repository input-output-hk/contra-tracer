{-|
Module      : Control.Tracer.Arrow
Copyright   : (c) Alexander Vieth, 2019
Licence     : Apache-2.0
Maintainer  : aovieth@gmail.com
-}

{-# LANGUAGE GADTSyntax #-}

module Control.Tracer.Arrow
  ( Tracer (..)
  , squelch
  , emit
  , mmap
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category

-- | A pure function which may also produce some side-channel value of type
-- @m@. When @m@ is a semigroup, we get an arrow. When it's a monoid, we get
-- an arrow with choice.
data Tracer m a b where
  Pure :: (a -> b     ) -> Tracer m a b
  Emit :: (a -> (b, m)) -> Tracer m a b

instance Semigroup m => Category (Tracer m) where

  id = Pure id

  Pure g . Pure f = Pure (g . f)
  Pure g . Emit f = Emit (first g . f)
  Emit g . Pure f = Emit (g . f)
  Emit g . Emit f = Emit $ \a ->
    let (b, m1) = f a
        (c, m2) = g b
    in  (c, m1 <> m2)

instance Semigroup m => Arrow (Tracer m) where

  arr = Pure

  Pure l *** Pure r = Pure $ \tup ->
    let a = l (fst tup)
        b = r (snd tup)
    in  (a, b)
  Pure l *** Emit r = Emit $ \tup ->
    let a      = l (fst tup)
        (b, m) = r (snd tup)
    in  ((a, b), m)
  Emit l *** Pure r = Emit $ \tup ->
    let (a, m) = l (fst tup)
        b      = r (snd tup)
    in  ((a, b), m)
  Emit l *** Emit r = Emit $ \tup ->
    let (a, m1) = l (fst tup)
        (b, m2) = r (snd tup)
    in  ((a, b), m1 <> m2)


instance Monoid m => ArrowChoice (Tracer m) where

  Pure l +++ Pure r = Pure (either (Left . l) (Right . r))
  Pure l +++ Emit r = Emit $ \choice -> case choice of
    Left  a -> let b      = l a in (Left  b, mempty)
    Right a -> let (b, m) = r a in (Right b, m)
  Emit l +++ Pure r = Emit $ \choice -> case choice of
    Left  a -> let (b, m) = l a in (Left  b, m)
    Right a -> let b      = r a in (Right b, mempty)
  Emit l +++ Emit r = Emit $ \choice -> case choice of
    Left  a -> let (b, m) = l a in (Left  b, m)
    Right a -> let (b, m) = r a in (Right b, m)

squelch :: Tracer m a ()
squelch = Pure (const ())

emit :: (a -> m) -> Tracer m a ()
emit f = Emit $ \a -> ((), f a)

mmap :: (m -> n) -> Tracer m a b -> Tracer n a b
mmap _ (Pure f) = Pure f
mmap h (Emit f) = Emit $ \a -> let (b, m) = f a in (b, h m)
