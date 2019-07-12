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
  { nodeCount :: NodeCount -- ^ The number of nodes
  , weights :: GraphWeights -- ^ The graph weights, will never change
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
