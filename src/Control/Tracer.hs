{-|
Module      : Control.Tracer
Description : A simple interface for logging, tracing, and monitoring
Copyright   : (c) IOHK, 2019
License     : Apache-2.0

=== General usage

'Tracer' is a contravariant functor intended to express the pattern in which
values of its parameter type are used to produce effects which are prescribed
by the caller, as in tracing, logging, code instrumentation, etc.

Programs should be written to use as specific a tracer as possible, i.e. to
take as a parameter a @Tracer m domainSpecificType@. To combine these programs
into an executable which does meaningful tracing, an implementation of that
tracing should be used to make a @Tracer probablyIO implementationTracingType@,
which is 'contramap'ped to fit @Tracer m domainSpecificType@ wherever it is
needed, for the various @domainSpecificType@s that appear throughout the
program.

=== An example

This short example shows how a tracer can be deployed, highlighting the use of
'contramap' to fit a general tracer which writes text to a file, where a
specific tracer which takes domain-specific events is expected.

> -- Writes text to some log file.
> traceToLogFile :: FilePath -> Tracer IO Text
>
> -- Domain-specific event type.
> data Event = EventA | EventB Int
>
> -- The log-file format for an Event.
> eventToText :: Event -> Text
>
> -- Some action that can use any tracer on Event, in any monad.
> actionWithTrace :: Monad m => Tracer m Event -> m ()
> actionWithTrace tracer = do
>   traceWith tracer EventA
>   traceWith tracer (EventB 42)
>
> -- Set up a log file tracer, then use it where the Event tracer is expected.
> main :: IO ()
> main = do
>   textTacer <- traceToLogFile "log.txt"
>   let eventTracer :: Tracer IO Event
>       eventTracer = contramap eventToText tracer
>   actionWithTrace eventTracer

-}

{-# LANGUAGE RankNTypes #-}

module Control.Tracer
    ( Tracer (..)
    , traceWith
    -- * Simple tracers
    , nullTracer
    , stdoutTracer
    , debugTracer
    -- * Transforming tracers
    , natTracer
    , contramapM
    , condTracing
    , condTracingM
    , showTracing
    , traceTraversable
    , traceAll
    -- * Re-export of Contravariant
    , Contravariant(..)
    ) where

import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Monoid (Monoid (..))
import           Data.Foldable (traverse_)
import           Debug.Trace (traceM)

-- | This type describes some effect in @m@ which depends upon some value of
-- type @a@, for which the /output value/ is not of interest (only the effects).
--
-- The motivating use case is to describe tracing, logging, monitoring, and
-- similar features, in which the programmer wishes to provide some values to
-- some /other/ program which will do some real world side effect, such as
-- writing to a log file or bumping a counter in some monitoring system.
--
-- The actual implementation of such a program will probably work on rather
-- large, domain-agnostic types like @Text@, @ByteString@, JSON values for
-- structured logs, etc.
-- But the call sites which ultimately /invoke/ these implementations will deal
-- with smaller, domain-specific types that concisely describe events, metrics,
-- debug information, etc.
--
-- This difference is reconciled by the 'Contravariant' instance for tracer.
-- 'Data.Functor.Contravariant.contramap' is used to change the input type of
-- a tracer. This allows for a more general tracer to be used where a more
-- specific one is expected.
--
-- Intuitively: if you can map your domain-specific type @Event@ to a @Text@
-- representation, then any @Tracer m Text@ can stand in where a
-- @Tracer m Event@ is required.
--
-- > eventToText :: Event -> Text
-- >
-- > traceTextToLogFile :: Tracer m Text
-- >
-- > traceEventToLogFile :: Tracer m Event
-- > traceEventToLogFile = contramap eventToText traceTextToLogFile
newtype Tracer m a = Tracer { runTracer :: a -> m () }

instance Contravariant (Tracer m) where
  contramap f = mapTracer (. f)

-- | @tr1 <> tr2@ will run @tr1@ and then @tr2@ with the same input.
instance Applicative m => Semigroup (Tracer m s) where
    Tracer a1 <> Tracer a2 = Tracer $ \s -> a1 s *> a2 s

instance Applicative m => Monoid (Tracer m s) where
    mappend = (<>)
    mempty  = nullTracer

-- | Alias for 'runTracer'. Traces the given value by way of the 'Tracer'.
traceWith :: Tracer m a -> a -> m ()
traceWith = runTracer

-- | Does nothing. Does not force its argument. Using this tracer effectively
-- "turns off" tracing. However, the value may still be forced, because this
-- tracer can still be 'contramap'ped with functions that are strict, such
-- as by 'condTracing'.
nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer $ \_ -> pure ()

-- | Similar in spirit to 'contramap', but the mapped function is a "Kleisli
-- arrow" (see Control.Arrow from base for further reading, but it's not
-- so important to know the special name). This is basically monadic bind in
-- front of (to the left of) the tracer's effect.
contramapM :: Monad m
           => (a -> m b)
           -> Tracer m b
           -> Tracer m a
contramapM f = mapTracer (f >=>)

-- | Use a predicate to filter traced values: if it gives false then the
-- tracer will not be run.
--
-- > condTracing p tr = Tracer $ \s -> when (p s) (traceWith tr s)
--
condTracing :: (Monad m) => (a -> Bool) -> Tracer m a -> Tracer m a
condTracing p tr = Tracer $ \s ->
    when (p s) (traceWith tr s)

-- | Like 'condTracing' but the "predicate" can do effects.
condTracingM :: (Monad m) => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = Tracer $ \s -> do
    active <- activeP
    when (active s) (traceWith tr s)

traceTraversable :: (Applicative m, Foldable t)
                 => Tracer m a -> Tracer m (t a)
traceTraversable = mapTracer traverse_

traceAll :: (Applicative m, Traversable t)
         => (b -> t a) -> Tracer m a -> Tracer m b
traceAll f = contramap f . traceTraversable

mapTracer :: ((a -> m ()) -> b -> n ())
          -> Tracer m a -> Tracer n b
mapTracer f (Tracer tr) = Tracer (f tr)

-- | Use a natural transformation to change the monad. This is useful, for
-- instance, to use concrete IO tracers in monad transformer stacks that have
-- IO as their base.
natTracer :: (forall x. m x -> n x)
          -> Tracer m a
          -> Tracer n a
natTracer f = mapTracer (f .)

-- | Trace strings to stdout. Output could be jumbled when this is used from
-- multiple threads. Consider 'debugTracer' instead.
stdoutTracer :: (MonadIO m) => Tracer m String
stdoutTracer = Tracer $ liftIO . putStrLn

-- | Trace strings using 'Debug.Trace.traceM'. This will use stderr. See
-- documentation in "Debug.Trace" for more details.
debugTracer :: (Applicative m) => Tracer m String
debugTracer = Tracer Debug.Trace.traceM

-- | Any tracer on strings is a tracer on types which are Show.
showTracing :: (Show a) => Tracer m String -> Tracer m a
showTracing = contramap show
