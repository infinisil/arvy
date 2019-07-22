module Evaluation.Utils where

import Data.Functor
import Prelude hiding (id, (.))
import Control.Monad
import Pipes
import Control.Applicative

-- Welford's online algorithm https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm
meanStddev :: forall n m a . Monad m => Floating n => Pipe n (n, n) m a
meanStddev = do
  v <- await
  go 1 v 0
  where
    go :: Int -> n -> n -> Pipe n (n, n) m a
    go count mean m2 = do
      v <- await
      let newCount = count + 1
          delta = v - mean
          newMean = mean + delta / fromIntegral newCount
          delta2 = v - newMean
          newM2 = m2 + delta * delta2

          stddev = sqrt $ newM2 / fromIntegral newCount
      yield (newMean, stddev)
      go newCount newMean newM2

enumerate :: forall n m a . Monad m => Pipe n (Int, n) m a
enumerate = go 0 where
  go :: Int -> Pipe n (Int, n) m a
  go n = do
    v <- await
    yield (n, v)
    go (n + 1)

everyNth :: forall n m a . Monad m => Int -> Pipe n n m a
everyNth n = go (n - 1) where
  go :: Int -> Pipe n n m a
  go 0 = await >>= yield >> go (n - 1)
  go k = void await >> go (k - 1)

decayingFilter :: forall a m x . Monad m => Int -> Pipe a a m x
decayingFilter r = go 0 0 (r - 1) where
  go :: Int -> Int -> Int -> Pipe a a m x
  go n p d = await >>= \event -> do
    when (p == 0) (yield event)
    case (p, d) of
      (0, 0) -> go (n + 1) (2 ^ (n + 1) - 1) r
      (0, _) -> go n (2 ^ n - 1) (d - 1)
      _ -> go n (p - 1) d

--lastOne :: forall a . Tracer a a
--lastOne = Tracer (Nothing :: Maybe a) \case
--  Nothing -> get >>= \case
--    Nothing -> return ()
--    Just value -> output value
--  Just value -> put $ Just value
--
--
--batch :: forall a . Int -> Tracer a [a]
--batch n = Tracer (0 :: Int, [] :: [a]) \case
--  Nothing -> get >>= \case
--    (0, _) -> return ()
--    (_, xs) -> output xs
--  Just value -> do
--    (k, xs) <- get
--    if k + 1 == n
--      then do
--        output $ reverse $ value : xs
--        put (0, [])
--      else modify (bimap (1+) (value:))
--
--meanStddevList :: Floating a => [a] -> (a, a)
--meanStddevList values = (mean, sqrt $ sum $ map (\v -> (v - mean) ^^ 2) values) where
--  mean = sum values / fromIntegral len
--  len = length values
