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
import Data.Bifunctor
import Polysemy.Trace
import Data.Array.IO
import Control.Applicative
import Arvy.Algorithm
import Arvy.Local
import Utils
import System.IO

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
  { evalStart :: Sem r s
  , evalRun :: Maybe ArvyEvent -> Sem (State s ': r) ()
  , evalEnd :: Sem (State s ': r) ()
  }

runEval :: (MArray arr Node m, Members '[Lift m] r) => Eval (Reader (Env arr) ': r) -> Env arr -> Sem (Output (Maybe ArvyEvent) ': r) () -> Sem r ()
runEval Eval { .. } env sem = runReader env $ do
  startState <- evalStart
  fmap snd $ runState startState $ do
    reinterpret2
      \case Output event -> evalRun event
      sem
    evalEnd

instance Semigroup (Eval r) where
  Eval st rn en <> Eval st' rn' en' = Eval
    { evalStart = liftA2 (,) st st'
    , evalRun = \event -> do
        mapStateFirst' $ rn event
        mapStateSecond' $ rn' event
    , evalEnd = do
        mapStateFirst' $ en
        mapStateSecond' $ en'
    }

instance Monoid (Eval r) where
  mempty = Eval
    { evalStart = return ()
    , evalRun = \_ -> return ()
    , evalEnd = return ()
    }


data Runner o r where
  RunTrace :: Member Trace r => (o -> String) -> Runner o r
  RunFile :: Member (Lift IO) r => FilePath -> (o -> String) -> Runner o r



runAs :: (MArray arr Node IO, Members '[Reader (Env arr), Lift IO] r) => Tracer ArvyEvent o -> Runner o r -> Eval r
runAs (Tracer s t) (RunTrace f) = Eval
  { evalStart = return (0 :: Int, s)
  , evalRun = \event -> do
      eventId <- gets fst
      modify (first (+1))

      interpret
        \case Output o -> trace $ "[" ++ show eventId ++ "] " ++ f o
        (mapStateSecond (t event))
  , evalEnd = return ()
  }
runAs (Tracer s t) (RunFile path f) = Eval
  { evalStart = do
      handle <- sendM $ openFile path WriteMode
      return (handle, s)
  , evalRun = \event -> do
      interpret
        \case Output o -> do
                handle <- gets fst
                sendM $ hPutStrLn handle (f o)
        (mapStateSecond (t event))
  , evalEnd = do
      handle <- gets fst
      sendM $ hClose handle
  }
{-


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
-}
