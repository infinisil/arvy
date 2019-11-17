module Evaluation.Utils where

import           Conduit
import           Control.Monad
import qualified Data.Conduit.Combinators as C
import           Data.List
import qualified Data.NonNull             as NN
import           Data.Sequence            (Seq, (|>))
import qualified Data.Sequence            as S
import           Evaluation.Types
import           Polysemy
import           Prelude

-- Welford's online algorithm https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm
meanStddev :: forall n m . Monad m => Floating n => ConduitT n (n, n) m ()
meanStddev = await >>= \case
  Nothing -> return ()
  Just v -> go 1 v 0
  where
    go :: Int -> n -> n -> ConduitT n (n, n) m ()
    go count mean m2 = await >>= \case
      Nothing -> return ()
      Just v -> do
        let newCount = count + 1
            delta = v - mean
            newMean = mean + delta / fromIntegral newCount
            delta2 = v - newMean
            newM2 = m2 + delta * delta2

            stddev = sqrt $ newM2 / fromIntegral newCount
        yield (newMean, stddev)
        go newCount newMean newM2

enumerate :: forall n m . Monad m => ConduitT n (Int, n) m ()
enumerate = go 1 where
  go :: Int -> ConduitT n (Int, n) m ()
  go n = await >>= \case
    Nothing -> return ()
    Just v -> do
      yield (n, v)
      go (n + 1)

everyNth :: forall n m . Monad m => Int -> ConduitT n n m ()
everyNth n = go (n - 1) where
  go :: Int -> ConduitT n n m ()
  go 0 = await >>= \case
    Nothing -> return ()
    Just v -> do
      yield v
      go (n - 1)
  go k = void await >> go (k - 1)

decayingFilter :: forall a m . Monad m => Int -> ConduitT a a m ()
decayingFilter r = go 0 0 (r - 1) where
  go :: Int -> Int -> Int -> ConduitT a a m ()
  go n p d = await >>= \case
    Nothing -> return ()
    Just event -> do
      when (p == 0) (yield event)
      case (p, d) of
        (0, 0) -> go (n + 1) (2 ^ (n + 1) - 1) r
        (0, _) -> go n (2 ^ n - 1) (d - 1)
        _      -> go n (p - 1) d

logFilter :: forall a m . Monad m => Env -> Int -> ConduitT a a m ()
logFilter Env { envRequestCount } count = go (NN.impureNonNull indices) 1 where

  lc :: Double = log (fromIntegral envRequestCount - 1)
  indices = map head $ group $ map (floor . exp) [0.0, lc / (fromIntegral count - 1) .. lc]

  go :: NN.NonNull [Int] -> Int -> ConduitT a a m ()
  go indcs k = await >>= \case
    Nothing -> return ()
    Just a -> do
      let (i, rest) = NN.nuncons indcs
      if i == k then do
        yield a
        case rest of
          Nothing -> awaitForever yield
          Just is -> go is (k + 1)
      else go indcs (k + 1)


movingAverage :: forall n m . (Monad m, Fractional n) => Bool -> Int -> ConduitT n n m ()
movingAverage initial window = await >>= \case
  Nothing -> return ()
  Just v -> go v (S.singleton v)
  where
    go :: n -> Seq n -> ConduitT n n m ()
    go mean hist
      | count < window = do
          when initial (yield mean)
          await >>= \case
            Nothing -> return ()
            Just v -> go ((mean * fromIntegral count + v) / (fromIntegral count + 1)) (hist |> v)
      | otherwise = go' mean hist
      where
        count = S.length hist
    go' :: n -> Seq n -> ConduitT n n m ()
    go' mean hist = do
      yield mean
      await >>= \case
        Nothing -> return ()
        Just v -> case S.viewl hist of
          S.EmptyL -> error "Shouldn't be empty"
          old S.:< rest -> do
            let newMean = mean + (v - old) / fromIntegral window
            go' newMean (rest |> v)

meanStddevList :: Floating a => [a] -> (a, a)
meanStddevList values = (mean, sqrt $ sum $ map (\v -> (v - mean) ^^ (2 :: Int)) values) where
  mean = sum values / fromIntegral len
  len = length values

asConduit :: Member (Lift IO) r => (Env -> IO a) -> (Env -> ConduitT e (e, a) (Sem r) ())
asConduit f env = C.mapM $ \event -> do
  result <- sendM $ f env
  return (event, result)
