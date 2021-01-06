{-|
Module      : Test.Control.Tracer.Properties
Description : Various essential properties for Tracer
Copyright   : (c) Alexander Vieth, 2019
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE Arrows #-}

module Test.Control.Tracer.Properties where

import Control.Arrow
import Control.Tracer

-- | Does not force the @error "failure"@ used within it, as it's not needed.
--
-- @traceWith laziness_1 anything = pure ()@
laziness_1 :: Monad m => Tracer m a
laziness_1 = arrow $ proc _ -> do
  _ <- arr not -< error "failure"
  returnA -< ()

-- | Simple choice: if both of 2 alternatives do not emit, nothing is forced.
-- 
-- @traceWith laziness_2 anything = pure ()@
laziness_2 :: Monad m => Tracer m a
laziness_2 = arrow $ arr (error "failure") >>> (use nullTracer ||| use nullTracer)

-- | Nested choice: even though the first choice _could_ emit, the second
-- choice can't, so nothing after the first choice is forced.
--
-- @traceWith laziness_3 anything = pure ()@
laziness_3 :: Monad m => Tracer m Bool
laziness_3 = arrow $ proc b ->
  if b
  then emit (const (pure ())) -< ()
  else do
    _ <- effect (error "failure") -< error "failure"
    _ <- use nullTracer ||| use nullTracer -< error "failure"
    returnA -< ()
