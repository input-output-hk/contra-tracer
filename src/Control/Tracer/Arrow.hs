{-|
Module      : Control.Tracer.Arrow
Copyright   : (c) Alexander Vieth, 2019
Licence     : Apache-2.0
Maintainer  : aovieth@gmail.com
-}

{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Arrows       #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Control.Tracer.Arrow
  ( Tracer (..)
  , compute
  , emit
  , effect
  , squelch
  , nat
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category

data Tracer m a b where
  -- | An emitting part, followed by a non-emitting part.
  -- The non-emitting part is there so that later emitting parts can be
  -- tacked-on later.
  Emitting   :: Kleisli m a x -> Kleisli m x b -> Tracer m a b
  -- | No emitting.
  Squelching :: Kleisli m a b                  -> Tracer m a b

squelch :: Applicative m => Tracer m a ()
squelch = compute (const ())

emit :: Applicative m => (a -> m ()) -> Tracer m a ()
emit f = Emitting (Kleisli f) (Kleisli (const (pure ())))

effect :: (a -> m b) -> Tracer m a b
effect = Squelching . Kleisli

compute :: Applicative m => (a -> b) -> Tracer m a b
compute f = effect (pure . f)

instance Monad m => Category (Tracer m) where
  id = compute id
  Squelching l     . Squelching r     = Squelching (l  . r)
  -- Crucial: the squelching parts stay together. Could also have written
  --                                  = Emitting   (rp . re)      l
  -- but that would miss opportunities to skip doing work.
  Squelching l     . Emitting   re rp = Emitting   re             (l . rp)
  -- Contrast with the above clause: here the emitting part comes _after_ the
  -- squelching part, so the squelching part becomes part of the emitting part.
  Emitting   le lp . Squelching r     = Emitting   (le . r)       lp
  Emitting   le lp . Emitting   re rp = Emitting   (le . rp . re) lp

instance Monad m => Arrow (Tracer m) where
  arr = compute
  Squelching l     *** Squelching r     = Squelching (l  *** r )
  Squelching l     *** Emitting   re rp = Emitting   (id *** re) (l  *** rp)
  Emitting   le lp *** Squelching r     = Emitting   (le *** id) (lp *** r )
  Emitting   le lp *** Emitting   re rp = Emitting   (le *** re) (lp *** rp)

instance Monad m => ArrowChoice (Tracer m) where
  Squelching l     +++ Squelching r     = Squelching (l +++ r)
  Squelching l     +++ Emitting   re rp = Emitting   (id +++ re) (l  +++ rp)
  Emitting   le lp +++ Squelching r     = Emitting   (le +++ id) (lp +++ r )
  Emitting   le lp +++ Emitting   re rp = Emitting   (le +++ re) (lp +++ rp)

nat :: (forall x . m x -> n x) -> Tracer m a b -> Tracer n a b
nat h (Squelching (Kleisli k))             = Squelching (Kleisli (h . k))
nat h (Emitting   (Kleisli k) (Kleisli l)) = Emitting   (Kleisli (h . k)) (Kleisli (h . l))
