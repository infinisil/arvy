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
import Data.Functor
import Polysemy.Trace
import Polysemy.Reader
import GHC.Generics
import Data.Monoid
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Arvy.Algorithm
import Arvy.Local
import Utils
import System.IO

-- | Readonly data available to evaluations
data Env arr = Env
  { weights :: GraphWeights -- ^ The graph weights, will never change
  , tree :: arr Node Node -- ^ The spanning tree, will change over time
  }

-- | A an evaluation function that takes some events i (or Nothing if no more events are coming) and outputs events o
data Tracer i o = forall s . Tracer
  { tracerInitialState :: s -- ^ The initial tracer state
  , tracerFun :: forall arr m r . (MArray arr Node m, Members '[Reader (Env arr), Lift m, State s, Output o] r) => Maybe i -> Sem r ()
  -- ^ What to do when an event occurs
  }

-- | 'Tracer' is a Category because the output of one tracer can be piped into another one
instance Category Tracer where
  id = Tracer () $ \case
    Nothing -> return ()
    Just event -> output event

  Tracer s t . Tracer s' t' = Tracer (s, s') $ \event -> do
    interpret
      \case Output o -> mapStateFirst (t (Just o))
      (mapStateSecond (t' event))
    mapStateFirst (t Nothing)

instance Functor (Tracer i) where
  fmap f (Tracer s t) = Tracer s \event -> interpret
    \case Output o -> output (f o)
    (t event)

-- | An 'Eval' is like a 'Tracer', but specific to 'ArvyEvent' inputs and the outputs get processed in some effect row @r@.
data Eval r = forall s . Eval
  { st :: s
  , evalFun :: Maybe ArvyEvent -> Sem (State s ': r) ()
  }

runEval :: Eval r -> Sem (Output (Maybe ArvyEvent) ': r) () -> Sem r ()
runEval (Eval s f) = fmap snd . runState s . reinterpret \case
  Output event -> f event

instance Semigroup (Eval r) where
  Eval s f <> Eval s' f' = Eval (s, s') $ \event -> do
    mapStateFirst' $ f event
    mapStateSecond' $ f' event

instance Monoid (Eval r) where
  mempty = Eval () $ \_ -> return ()

-- | Function for coercing a 'Tracer' for 'ArvyEvent's into an 'Eval' with a function that acts upon the 'Tracer's outputs.
runAs :: (MArray arr Node m, Members '[Reader (Env arr), Lift m] r) => Tracer ArvyEvent o -> (o -> Sem r ()) -> Eval r
runAs (Tracer s t) f = Eval s $ \event -> do
  interpret
    \case Output o -> raise $ f o
    (t event)
  return ()

summing :: forall n . Num n => Tracer n n
summing = Tracer (0 :: n) $ \case
  Nothing -> return ()
  Just value -> do
    total <- gets (+value)
    put total
    output total

test :: Tracer ArvyEvent Int
test = Tracer () $ \case
  Nothing -> output 1
  Just event -> output 2

testcomb :: Tracer ArvyEvent Int
testcomb = summing . test

testrunners :: (MArray arr Node m, Members '[Trace, Reader (Env arr), Lift m] r) => Eval r
testrunners = mconcat
  [ (summing . test) `runAs` (\v -> trace (show v))
  , hopCount `runAs` (\hops -> trace $ "Hops: " ++ show hops)
  ]

hopCount :: Tracer ArvyEvent Int
hopCount = requests (\_ _ -> Sum 1) <&> getSum . path


outputting :: Member (Output (Maybe ArvyEvent)) r => Sem r ()
outputting = do
  output $ Just $ RequestMade 0
  output $ Just $ RequestTravel 0 1 ""
  output $ Just $ RequestTravel 1 2 ""
  output $ Just $ RequestGranted 0 2 Arvy.Local.Local
  output Nothing

testit :: IO ()
testit = do
  let weights = listArray ((0, 0), (0, 0)) []
  tree <- newArray (0, 0) 0 :: IO (IOArray Node Node)
  runM $ runTraceIO $ runReader (Env weights tree) (runEval testrunners outputting)

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show, Generic)

requests :: forall a . Monoid a => (Node -> Node -> a) -> Tracer ArvyEvent (Request a)
requests f = Tracer (0 :: Int, mempty :: a) \case
  Just (RequestMade requestFrom) -> put (requestFrom, mempty)
  Just (RequestGranted _ root _) -> do
    (requestFrom, as) <- get
    output $ Request requestFrom root as
  Just (RequestTravel x y _) -> modify $ second (f x y <>)
  _ -> return ()

averaging :: forall n . Fractional n => Tracer n n
averaging = Tracer (0 :: n, 0 :: Int) \case
  Nothing -> do
    (values, count) <- get
    output $ values / fromIntegral count
  Just value -> modify $ bimap (+value) (+1)


{- |
To evaluate:
- Tree diameter
-

-}

{- |
How about just making every evaluation take ArvyEvent inputs and outputting some custom type, or resulting in some final value. Have a verbosity flag for controlling whether they output something?

-}
{-
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



--
--testit :: IO ()
--testit = runM $ runEval (hopCount (\v -> sendM $ print v)) outputting

hopCount :: (Maybe Int -> Sem r ()) -> Eval r
hopCount f = Eval () $ reinterpret (\case
  Output (Just (Request { path = Sum v })) -> raise (f (Just v))
  Output Nothing -> raise (f Nothing))
  . collectRequests (\_ _ -> Sum (1 :: Int))



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
-}
