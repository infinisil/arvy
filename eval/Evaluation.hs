{-# LANGUAGE DeriveFunctor #-}
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
import Arvy.Weights
import Data.Monoid
import Data.Array.MArray
import Arvy.Utils
import Arvy.Algorithm
import Arvy.Tree
import Control.DeepSeq


-- | An evaluation that takes an input i and computes some outputs o
data Eval i o = forall s . (NFData s) => Eval
  { initialState :: s
  , tracing :: forall r . Members '[State s, Output o] r => Tree -> i -> Sem r ()
  , final :: forall r . Members '[State s, Output o] r => Tree -> Sem r ()
  }

{-# INLINE runEval #-}
runEval :: forall m r a i o x . (NFData o, Members '[Lift m, Output o] r, MArray a (Maybe Int) m) => Eval i o -> a Int (Maybe Int) -> Sem (Output (Maybe i) ': r) x -> Sem r x
runEval Eval { .. } mtree = fmap snd . runState initialState . reinterpret \case
  Output mi -> do
    s <- get
    -- TODO: See if unsafeFreeze works here
    frozen <- sendM $ freeze mtree
    let (outs, (s', _)) = force $ run $ runFoldMapOutput (:[]) $ runState s $ case mi of
          Nothing -> final frozen
          Just i -> tracing frozen i
    put s'
    traverse output outs
    return $! seq outs ()

aggregate :: forall m . (NFData m, Monoid m) => Eval m m
aggregate = Eval
  { initialState = mempty :: m
  , tracing = \_ v -> do
      modify (<>v)
  , final = \_ -> get >>= output
  }

average :: forall m . (NFData m, Fractional m) => Eval m m
average = Eval
  { initialState = (0, 0) :: (m, Int)
  , tracing = \_ v -> modify (bimap (+v) (+1))
  , final = \_ -> get >>= \(v, c) -> output (v / fromIntegral c)
  }

requests :: forall a . (NFData a, Monoid a) => (Int -> Int -> a) -> Eval ArvyEvent (Request a)
requests f = Eval
  { initialState = (0, mempty) :: (Int, a)
  , tracing = \_ event -> case event of
      RequestMade requestFrom -> put $ (requestFrom, mempty)
      RequestGranted grant -> do
        let requestRoot = case grant of
              AlreadyHere root -> root
              GottenFrom _ root -> root
        (requestFrom, as) <- get
        output $ Request requestFrom requestRoot as
      RequestTravel x y _ -> modify $ second (f x y <>)
      _ -> return ()
  , final = \_ -> return ()
  }

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show, Generic, NFData)

requestHops :: Eval ArvyEvent (Request (Sum Int))
requestHops = requests (\_ _ -> Sum 1)

treeStretch :: Int -> GraphWeights -> Eval ArvyEvent Double
treeStretch n weights = Eval
  { initialState = ()
  , tracing = \tree event -> case event of
      RequestMade _ -> output $ avgTreeStretch n weights tree
      _ -> return ()
  , final = \tree -> output $ avgTreeStretch n weights tree
  }

everyNth :: Int -> Eval i i
everyNth n = Eval
  { initialState = n - 1
  , tracing = \_ event -> get >>= \case
      0 -> do
        put (n - 1)
        output event
      k -> do
        put (k - 1)
        return ()
  , final = \_ -> return ()
  }

instance Functor (Eval i) where
  fmap f (Eval i t fi) = Eval
    { initialState = i
    , tracing = \tree event ->
        interpret
          \case Output o -> output $ f o
          $ t tree event
    , final = \tree ->
        interpret
          \case Output o -> output $ f o
          $ fi tree
    }

ignoring :: Eval a b
ignoring = Eval
  { initialState = ()
  , tracing = \_ _ -> return ()
  , final = \_ -> return ()
  }

combine :: Eval a b -> Eval a c -> Eval a (Either b c)
(Eval i1 t1 f1) `combine` (Eval i2 t2 f2) = Eval
  { initialState = (i1, i2)
  , tracing = \tree event -> do
      interpret
        \case Output o -> output (Left o)
        $ mapStateFirst (t1 tree event)
      interpret
        \case Output o -> output (Right o)
        $ mapStateSecond (t2 tree event)
  , final = \tree -> do
      interpret
        \case Output o -> output (Left o)
        $ mapStateFirst (f1 tree)
      interpret
        \case Output o -> output (Right o)
        $ mapStateSecond (f2 tree)
  }

instance Category Eval where
  id = Eval
    { initialState = ()
    , tracing = \_ event -> output event
    , final = \_ -> return ()
    }
  (Eval i2 t2 f2) . (Eval i1 t1 f1) = Eval
    { initialState = (i1, i2)
    , tracing = \tree event ->
        interpret
          \case Output o -> do
                  mapStateSecond (t2 tree o)
          $ mapStateFirst (t1 tree event)
    , final = \tree ->
        interpret
          \case Output o ->
                  mapStateSecond $ do
                    t2 tree o
                    f2 tree
          $ mapStateFirst (f1 tree)
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
