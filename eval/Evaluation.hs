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
import Polysemy.Output
import Polysemy.Reader
import Data.Array.IO
import Arvy.Algorithm
import Arvy.Local
import Utils
import Control.Exception
import Data.Time

-- | Readonly data available to evaluations
data Env (arr :: * -> * -> *) = Env (arr Node Node)

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

  Tracer s t . Tracer s' t' = Tracer (s, s') $ \case
    Nothing -> do
      interpret
        \case Output o -> mapStateFirst (t (Just o))
        (mapStateSecond (t' Nothing))
      mapStateFirst (t Nothing)
    event -> interpret
      \case Output o -> mapStateFirst (t (Just o))
      (mapStateSecond (t' event))

instance Functor (Tracer i) where
  fmap f (Tracer s t) = Tracer s \event -> interpret
    \case Output o -> output (f o)
    (t event)

-- | An 'Eval' is like a 'Tracer', but specific to 'ArvyEvent' inputs and the outputs get processed in some effect row @r@.
data Eval r = forall s . Eval
  { st :: s
  , evalFun :: Maybe ArvyEvent -> Sem (State s ': r) ()
  }

runEval :: (MArray arr Node m, Members '[Lift m] r) => Eval (Reader (Env arr) ': r) -> Env arr -> Sem (Output (Maybe ArvyEvent) ': r) () -> Sem r ()
runEval (Eval s f) env = runReader env . fmap snd . runState s . reinterpret2 \case
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

-- | Function for coercing a 'Tracer' for 'ArvyEvent's into an 'Eval' with a function that acts upon the 'Tracer's outputs.
runTimingAs :: (MArray arr Node IO, Members '[Reader (Env arr), Lift IO] r) => Tracer ArvyEvent o -> (o -> Sem r ()) -> Eval r
runTimingAs (Tracer s t) f = Eval s $ \event -> do
  interpret
    \case Output o -> do
            start <- sendM getCurrentTime
            raise $ f o
            end <- sendM getCurrentTime
            sendM $ putStrLn $ "Time needed: " ++ show (end `diffUTCTime` start)
    (t event)
