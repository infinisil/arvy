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
import GHC.Generics
import Data.Monoid
import Arvy.Algorithm
import Arvy.Local
import Control.DeepSeq
import Utils


-- | An evaluation that takes an input i and computes some outputs o
data Eval i o = forall s . (NFData s) => Eval
  { initialState :: s
  , tracing :: forall r . Members '[State s, Output o, Lift IO] r => i -> Sem r ()
  , final :: forall r . Members '[State s, Output o, Lift IO] r => Sem r ()
  }

{-# INLINE runEval #-}
runEval :: forall r i o x . (Members '[Lift IO, Output o] r) => Eval i o -> Sem (Output (Maybe i) ': r) x -> Sem r x
runEval Eval { .. } = fmap snd . runState initialState . reinterpret \case
  Output mi -> do
    case mi of
      Just i -> tracing i
      Nothing -> final

aggregate :: forall m . (NFData m, Monoid m) => Eval m m
aggregate = Eval
  { initialState = mempty :: m
  , tracing = \v -> do
      modify (<>v)
  , final = get >>= output
  }

average :: forall m . (NFData m, Fractional m) => Eval m m
average = Eval
  { initialState = (0, 0) :: (m, Int)
  , tracing = \v -> modify (bimap (+v) (+1))
  , final = get >>= \(v, c) -> output (v / fromIntegral c)
  }

collectRequests :: forall a . (NFData a, Monoid a) => (Int -> Int -> a) -> Eval ArvyEvent (Request a)
collectRequests f = Eval
  { initialState = (0, mempty) :: (Int, a)
  , tracing = \event -> case event of
      RequestMade requestFrom -> put $ (requestFrom, mempty)
      RequestGranted _ root _ -> do
        (requestFrom, as) <- get
        output $ Request requestFrom root as
      RequestTravel x y _ -> modify $ second (f x y <>)
      _ -> return ()
  , final = return ()
  }

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show, Generic, NFData)

requestHops :: Eval ArvyEvent (Request (Sum Int))
requestHops = collectRequests (\_ _ -> Sum 1)


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
    , final = interpret
          \case Output o ->
                  mapStateSecond $ do
                    t2 o
                    f2
          $ mapStateFirst f1
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
