{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Tracer
import Data.Foldable (foldlM)
import Data.Word
import System.Environment (getArgs)

fibonaccis :: [Integer]
fibonaccis = 1 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

isPrime :: Integer -> Bool
isPrime n | n == 1    = False
          | otherwise = not (any (divides n) [2..(n `div` 2)])

divides :: Integer -> Integer -> Bool
divides n m = rem n m == 0

-- Transform any tracer on Integers so that it traces only prime numbers.
--
-- This is the key part of the demo. Using the original representation of
--
-- @Tracer m a ~ Integer -> m ()@
--
-- the fact that this is an alternative between the argument tracer and
-- @squelch@ would be lost. But with the arrow representation, it's still known,
-- so that when we substitute @nullTracer@ in @main@, the whole thing becomes
-- a no-op.
primeTracer :: Monad m => Tracer m Integer -> Tracer m Integer
primeTracer tracer = arrow $ proc i ->
  if isPrime i then use tracer -< i else squelch -< ()

-- Or try this next one: when the tracer is stdoutTracer it will print for
-- every fibonacci number, even if it's not prime, but if it's the nullTracer
-- then it will realize the whole thing will never do any tracing and so it
-- skips the side-effect. The arrow representation is crucial: we don't need to
-- run the effect (putStrLn) in order to determine whether there will be any
-- trace effects.
--
-- Useful application: getting the current time and thread id to pass to a
-- tracer. If tracing is flipped off, you skip those calls.
primeTracer' :: Tracer IO Integer -> Tracer IO Integer
primeTracer' tracer = arrow $ proc i -> do
  () <- effect putStrLn -< "doing some IO lol"
  if isPrime i then use tracer -< i else squelch -< ()

main :: IO ()
main = do
  [numFibsStr, traceStr] <- getArgs
  numFibs <- case reads numFibsStr of
    [(numFibs :: Word, "")] -> pure numFibs
    _ -> error "must specify a Word followed by a Bool"
  trace <- case reads traceStr of
    [(trace :: Bool, "")] -> pure trace
    _ -> error "must specify a Word followed by a Bool"
  let -- If the second command-line argument is True, we'll trace to stdout.
      -- Otherwise, do not trace (nullTracer).
      tracer :: Tracer IO Integer
      tracer = contramap show (if trace then stdoutTracer else nullTracer)
      -- To fold over a certain prefix of the fibonacci numbers. It will
      -- sum them, but also trace every one that is prime, using the
      -- configured tracer.
      -- The key point is that if tracing is disabled (second CLI argument is
      -- False), then we don't even bother checking whether the thing is prime.
      summarize :: Integer -> Integer -> IO Integer
      summarize !total i = do
        -- GHC will float out @primeTracer tracer@ so that it's only evaluted
        -- once.
        traceWith (primeTracer tracer) i
        pure (total + i)
  total <- foldlM summarize 0 (take (fromIntegral numFibs) fibonaccis)
  print total
