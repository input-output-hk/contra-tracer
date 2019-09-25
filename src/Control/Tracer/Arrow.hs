{-|
Module      : Control.Tracer.Arrow
Copyright   : (c) Alexander Vieth, 2019
Licence     : Apache-2.0
Maintainer  : aovieth@gmail.com
-}

{-# LANGUAGE GADTSyntax   #-}
{-# LANGUAGE Arrows       #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Control.Tracer.Arrow
  ( Tracer (..)
  , squelch
  , emit
  , effect
  , mmap
  , kmap
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.Fix (MonadFix)

-- | A Kleisli arrow in @f@ which may also produce some side-channel value of
-- type @m@. When @m@ is a semigroup, we get an arrow. When it's a monoid, we
-- get an arrow with choice (assuming @f@ is a monad).
--
-- It is deliberately not ArrowApply, ArrowZero, or ArrowPlus.
-- ArrowApply is not compatible with statically knowing whether a Tracer
-- will emit anything.
data Tracer m f a b where
  Pure :: Kleisli f a b      -> Tracer m f a b
  Emit :: Kleisli f a (b, m) -> Tracer m f a b

instance (Semigroup m, Monad f) => Category (Tracer m f) where

  id = Pure id

  Pure g . Pure f = Pure (g . f)
  Pure g . Emit f = Emit (first g . f)
  Emit g . Pure f = Emit (g . f)
  Emit g . Emit f = Emit $ proc a -> do
    (b, !m1) <- f -< a
    (c, !m2) <- g -< b
    returnA -< (c, (m1 <>) $! m2)

instance (Semigroup m, Monad f) => Arrow (Tracer m f) where

  arr = Pure . arr

  Pure l *** Pure r = Pure $ l *** r
  Pure l *** Emit r = Emit $ proc (a, a') -> do
    b        <- l -< a
    (b', !m) <- r -< a'
    returnA -< ((b, b'), m)
  Emit l *** Pure r = Emit $ proc (a, a') -> do
    (b, !m) <- l -< a
    b'      <- r -< a'
    returnA -< ((b, b'), m)
  Emit l *** Emit r = Emit $ proc (a, a') -> do
    (b, !m)   <- l -< a
    (b', !m') <- r -< a'
    returnA -< ((b, b'), (m <>) $! m')


instance (Monoid m, Monad f) => ArrowChoice (Tracer m f) where

  Pure l +++ Pure r = Pure $ l +++ r
  Pure l +++ Emit r = Emit $ proc choice -> do
    case choice of
      Left  b  -> do
        c <- l -< b
        returnA -< (Left c, mempty)
      Right b' -> do
        (c', !m) <- r -< b'
        returnA -< (Right c', m)
  Emit l +++ Pure r = Emit $ proc choice -> do
    case choice of
      Left  b  -> do
        (c, !m) <- l -< b
        returnA -< (Left c, m)
      Right b' -> do
        c' <- r -< b'
        returnA -< (Right c', mempty)
  Emit l +++ Emit r = Emit $ proc choice -> do
    case choice of
      Left  b  -> do
        (c, !m) <- l -< b
        returnA -< (Left c, m)
      Right b' -> do
        (c', !m) <- r -< b'
        returnA -< (Right c', m)

instance (Semigroup m, MonadFix f) => ArrowLoop (Tracer m f) where
  loop (Pure f) = Pure $ loop f
  loop (Emit f) = Emit $ loop $ proc (b, d) -> do
    ((c, m), d') <- f -< (b, d)
    returnA -< ((c, d'), m)

squelch :: (Monad f) => Tracer m f a ()
squelch = Pure (arr (const ()))

emit :: (Monad f) => (a -> m) -> Tracer m f a ()
emit f = Emit (arr (\a -> ((), f a)))

effect :: (a -> f b) -> Tracer m f a b
effect = Pure . Kleisli

mmap :: (Monad f) => (m -> n) -> Tracer m f a b -> Tracer n f a b
mmap _ (Pure f) = Pure f
mmap h (Emit f) = Emit $ proc a -> do
  (b, m) <- f -< a
  returnA -< (b, h $! m)

kmap :: (forall x . f x -> g x) -> Tracer m f a b -> Tracer m g a b
kmap h (Pure (Kleisli k)) = Pure (Kleisli (h . k))
kmap h (Emit (Kleisli k)) = Emit (Kleisli (h . k))
