module Evaluation.Utils where

import Data.Functor
import Prelude hiding (id, (.))
import Control.Monad
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>))

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

distribute :: (Monad m, Foldable f) => f (Consumer i m ()) -> Consumer i m ()
distribute = foldr (\con rest -> P.tee con >-> rest) P.drain

chunk :: Monad m => Int -> Pipe i [i] m ()
chunk k = replicateM k await >>= yield >> chunk k

movingAverage :: forall n m . (Monad m, Fractional n) => Bool -> Int -> Pipe n n m ()
movingAverage initial window = do
  v <- await
  go v (S.singleton v)
  where
    go :: n -> Seq n -> Pipe n n m ()
    go mean hist
      | count < window = do
          when initial (yield mean)
          v <- await
          go ((mean * fromIntegral count + v) / (fromIntegral count + 1)) (hist |> v)
      | otherwise = go' mean hist
      where
        count = S.length hist
    go' :: n -> Seq n -> Pipe n n m ()
    go' mean hist = do
      yield mean
      v <- await
      case S.viewl hist of
        S.EmptyL -> error "Shouldn't be empty"
        old S.:< rest -> do
          let newMean = mean + (v - old) / fromIntegral window
          go' newMean (rest |> v)

meanStddevList :: Floating a => [a] -> (a, a)
meanStddevList values = (mean, sqrt $ sum $ map (\v -> (v - mean) ^^ (2 :: Int)) values) where
  mean = sum values / fromIntegral len
  len = length values
