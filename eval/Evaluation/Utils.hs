module Evaluation.Utils where

import Evaluation
import Polysemy.State
import Polysemy.Output
import Data.Bifunctor
import Data.Functor
import Prelude hiding (id)
import Control.Category
import Control.Monad

logging :: Show a => Tracer a String
logging = id <&> show

filtering :: (a -> Bool) -> Tracer a a
filtering f = Tracer () \case
  Nothing -> return ()
  Just event -> when (f event) (output event)

-- Welford's online algorithm https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm
meanStddev :: forall m . Floating m => Tracer m (m, m)
meanStddev = Tracer (Nothing :: Maybe (Int, m, m)) \case
  Just v -> get >>= \case
    Nothing -> put $ Just (1, v, 0)
    Just (count, mean, m2) -> do
      let newCount = count + 1
          delta = v - mean
          newMean = mean + delta / fromIntegral newCount
          delta2 = v - newMean
          newM2 = m2 + delta * delta2
      put $ Just (newCount, newMean, newM2)
  Nothing -> get >>= \case
    Nothing -> return ()
    Just (1, _, _) -> return ()
    Just (count, mean, m2) -> do
      let stddev = sqrt $ m2 / fromIntegral count
      output (mean, stddev)


averaging :: forall n . Fractional n => Tracer n n
averaging = Tracer (0 :: n, 0 :: Int) \case
  Nothing -> do
    (values, count) <- get
    output $ values / fromIntegral count
  Just value -> modify $ bimap (+value) (+1)

summing :: forall n . Num n => Tracer n n
summing = Tracer (0 :: n) $ \case
  Nothing -> return ()
  Just value -> do
    total <- gets (+value)
    put total
    output total

everyNth :: Int -> Tracer n n
everyNth n = Tracer (n - 1) \case
  Nothing -> return ()
  Just event -> get >>= \case
    0 -> do
      put (n - 1)
      output event
    k -> do
      put (k - 1)