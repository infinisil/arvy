{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Evaluation where

import Prelude hiding ((.), id)
import Polysemy
import Control.Category
import Data.Bifunctor
import Polysemy.Output
import Polysemy.Input
import Polysemy.Trace
import GHC.Generics
import Data.Monoid
import Data.Array.IArray
import Arvy.Algorithm
import Arvy.Local
import Utils
import System.IO

{- |
How about just making every evaluation take ArvyEvent inputs and outputting some custom type, or resulting in some final value. Have a verbosity flag for controlling whether they output something?

-}

data Eval r = forall s . Eval s (forall a . Sem (Output (Maybe ArvyEvent) ': r) a -> Sem (State s ': r) a)

runEval :: Eval r -> Sem (Output (Maybe ArvyEvent) ': r) a -> Sem r a
runEval (Eval s f) = fmap snd . runState s . f

instance Semigroup (Eval r) where
  Eval s1 f1 <> Eval s2 f2 = Eval (s1, s2) $ \sem -> do
    mapStateFirst (f1 sem)
    mapStateSecond (f2 sem)

instance Monoid (Eval r) where
  mempty = Eval () $ reinterpret \case
    Output _ -> return ()

debug :: Member Trace r => Eval r
debug = statelessEval $ interpret \case
  Output (Just event) -> trace $ show event
  Output Nothing -> return ()

statelessEval :: (forall a . Sem (Output (Maybe ArvyEvent) ': r) a -> Sem r a) -> Eval r
statelessEval f = Eval () (raise . f)


data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show, Generic)
  
collectRequests :: forall a r x . Monoid a => (Node -> Node -> a) -> Sem (Output (Maybe ArvyEvent) ': r) x -> Sem (Output (Maybe (Request a)) ': r) x
collectRequests f = fmap snd . runState (0 :: Int, mempty :: a) . reinterpret2 \case
  Output Nothing -> output Nothing
  Output (Just (RequestMade requestFrom)) -> put (requestFrom, mempty)
  Output (Just (RequestGranted _ root _)) -> do
    (requestFrom, as) <- get
    output $ Just $ Request requestFrom root as
  Output (Just (RequestTravel x y _)) -> modify $ second (f x y <>)
  Output _ -> return ()

outputting :: Member (Output (Maybe ArvyEvent)) r => Sem r ()
outputting = do
  output $ Just $ RequestMade 0
  output $ Just $ RequestTravel 0 1 ""
  output $ Just $ RequestTravel 1 2 ""
  output $ Just $ RequestGranted 0 2 Local
  output Nothing

testit :: IO ()
testit = runM $ runEval (hopCount (\v -> sendM $ print v)) outputting
  
hopCount :: (Maybe Int -> Sem r ()) -> Eval r
hopCount f = Eval () $ reinterpret (\case
  Output (Just (Request { path = Sum v })) -> raise (f (Just v))
  Output Nothing -> raise (f Nothing))
  . collectRequests (\_ _ -> Sum (1 :: Int))

averaging :: forall x n r . (Fractional n, Show n, Monoid x, Member Trace r) => (Node -> Node -> x) -> (Request x -> n) -> Eval r
averaging p f = Eval (0 :: n, 0 :: Int) $ reinterpret
  \case
    Output (Just req) -> modify (bimap (+ f req) (+1))
    Output Nothing -> do
      (values, count) <- get
      trace $ show $ values / fromIntegral count
  . collectRequests p


eventToValue :: (ArvyEvent -> x) -> Sem (Output (Maybe ArvyEvent) ': r) a -> Sem (Output (Maybe x) ': r) a
eventToValue f = reinterpret \case
  Output Nothing -> output Nothing
  Output (Just event) -> output $ Just (f event)
{-

average :: forall m . Fractional m => Eval m m
average = Eval
  { initialState = (0, 0) :: (m, Int)
  , tracing = \v -> modify (bimap (+v) (+1))
  , final = get >>= \(v, c) -> output (v / fromIntegral c)
  }

-- Welford's online algorithm https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm
meanStddev :: forall m . Floating m => Eval m (m, m)
meanStddev = Eval
  { initialState = Nothing :: Maybe (Int, m, m)
  , tracing = \v -> get >>= \case
      Nothing -> put $ Just (1, v, 0)
      Just (count, mean, m2) -> do
        let newCount = count + 1
            delta = v - mean
            newMean = mean + delta / fromIntegral newCount
            delta2 = v - newMean
            newM2 = m2 + delta * delta2
        put $ Just (newCount, newMean, newM2)
  , final = get >>= \case
      Nothing -> return ()
      Just (1, _, _) -> return ()
      Just (count, mean, m2) -> do
        let stddev = sqrt $ m2 / fromIntegral count
        output (mean, stddev)
  }


evalFilter :: (b -> Bool) -> Eval b b
evalFilter f = Eval
  { initialState = ()
  , tracing = \event -> if f event
    then output event
    else return ()
  , final = return ()
  }

ratio :: GraphWeights -> Eval ArvyEvent Double
ratio weights = (\Request { path = Sum path, requestFrom = a, requestRoot = b } -> path / weights ! (a, b))
  <$> (collectRequests (\a b -> Sum (weights ! (a, b))) >>> evalFilter (\Request { .. } -> requestFrom /= requestRoot))


requestHops :: Eval ArvyEvent (Request (Sum Int))
requestHops = collectRequests (\_ _ -> Sum 1)

enumerate :: Eval a (Int, a)
enumerate = Eval
  { initialState = 0 :: Int
  , tracing = \event -> do
      counter <- get
      output (counter, event)
      put (counter + 1)
  , final = return ()
  }

everyNth :: Int -> Eval i i
everyNth n = Eval
  { initialState = n - 1
  , tracing = \event -> get >>= \case
      0 -> do
        put (n - 1)
        output event
      k -> do
        put (k - 1)
        return ()
  , final = return ()
  }

instance Functor (Eval i) where
  fmap f (Eval i t fi) = Eval
    { initialState = i
    , tracing = \event ->
        interpret
          \case Output o -> output $ f o
          $ t event
    , final = interpret
          \case Output o -> output $ f o
          $ fi
    }

ignoring :: Eval a b
ignoring = Eval
  { initialState = ()
  , tracing = \_ -> return ()
  , final = return ()
  }

combine :: Eval a b -> Eval a c -> Eval a (Either b c)
(Eval i1 t1 f1) `combine` (Eval i2 t2 f2) = Eval
  { initialState = (i1, i2)
  , tracing = \event -> do
      interpret
        \case Output o -> output (Left o)
        $ mapStateFirst (t1 event)
      interpret
        \case Output o -> output (Right o)
        $ mapStateSecond (t2 event)
  , final = do
      interpret
        \case Output o -> output (Left o)
        $ mapStateFirst f1
      interpret
        \case Output o -> output (Right o)
        $ mapStateSecond f2
  }

instance Category Eval where
  id = Eval
    { initialState = ()
    , tracing = \event -> output event
    , final = return ()
    }
  (Eval i2 t2 f2) . (Eval i1 t1 f1) = Eval
    { initialState = (i1, i2)
    , tracing = \event ->
        interpret
          \case Output o -> do
                  mapStateSecond (t2 o)
          $ mapStateFirst (t1 event)
    , final = do
        interpret
          \case Output o ->
                  mapStateSecond $ t2 o
          $ mapStateFirst f1
        mapStateSecond f2
    }
  
{-

measureRatio :: Member Trace r => GraphWeights -> GraphWeights -> Sem (Output ArvyEvent ': r) a -> Sem (Output ArvyEvent ': Output Double ': r) (Int, a)
measureRatio weights shortestPaths = fmap (\((_, n), a) -> (n, a)) . runState (0.0 :: Double, 0) . reinterpret3 \case
  Output event -> do
    output event
    case event of
      RequestMade _ -> do
        modify @(Double, Int) $ \(_, n) -> (0.0, n)
      RequestTravel a b _ -> do
        modify @(Double, Int) $ \(d, n) -> (d + weights ! (a, b), n)
      RequestGranted (GottenFrom i src) -> do
        (pathLength, _) <- get @(Double, Int)
        modify @(Double, Int) $ \(d, n) -> (d, n + 1)
        output $ pathLength / shortestPaths ! (i, src)
      _ -> return ()

-}
-}
