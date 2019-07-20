module Evaluation.Utils where

import Evaluation
import Polysemy.State
import Polysemy.Output
import Data.Functor
import Prelude hiding (id)
import Control.Category
import Control.Monad
import Data.Bifunctor

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
      let stddev = sqrt $ newM2 / fromIntegral newCount
      output (newMean, stddev)
  Nothing -> return ()

enumerate :: Tracer a (Int, a)
enumerate = Tracer (0 :: Int) \case
  Nothing -> return ()
  Just value -> do
    n <- get
    output (n, value)
    put (n + 1)

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

decayingFilter :: Int -> Tracer a a
decayingFilter r = Tracer (0 :: Int, 0 :: Int, r - 1) \case
  Nothing -> return ()
  Just event -> do
    (n, p, d) <- get
    when (p == 0) (output event)
    (newN, newP, newD) <- if p == 0
      then
        return $ if d == 0
          then (n + 1, 2 ^ (n + 1) - 1, r)
          else (n, 2 ^ n - 1, d - 1)
      else return (n, p - 1, d)
    put (newN, newP, newD)

lastOne :: forall a . Tracer a a
lastOne = Tracer (Nothing :: Maybe a) \case
  Nothing -> get >>= \case
    Nothing -> return ()
    Just value -> output value
  Just value -> put $ Just value


batch :: forall a . Int -> Tracer a [a]
batch n = Tracer (0 :: Int, [] :: [a]) \case
  Nothing -> get >>= \case
    (0, _) -> return ()
    (_, xs) -> output xs
  Just value -> do
    (k, xs) <- get
    if k + 1 == n
      then do
        output $ reverse $ value : xs
        put (0, [])
      else modify (bimap (1+) (value:))

meanStddevList :: Floating a => [a] -> (a, a)
meanStddevList values = (mean, sqrt $ sum $ map (\v -> (v - mean) ^^ 2) values) where
  mean = sum values / fromIntegral len
  len = length values
