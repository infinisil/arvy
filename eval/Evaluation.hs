{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Evaluation where

import Prelude hiding ((.), id)
import Polysemy
import Polysemy.Trace
import Polysemy.Output
import Arvy.Weights
import Data.Monoid
import Data.Array.MArray
import Data.Array.IArray
import Data.Array (Array)
import Arvy.Utils
import Arvy.Algorithm
import Data.Array.Unboxed ((!))
import Control.Category

import Data.Void
import Polysemy.Reader

{- |

What can we evaluate?
- Request path length, distance and hops
- Shortest path between token travels
- How good the tree is over time


- Combinations of above, like ratio, averages, sums, smoothed versions

How can this be nicely represented?

Two types:
- Continuous evaluations (=? Output)
- Aggregated evaluations (=? Result)

How should the results be reported?
- Could just log them all, which would make it very simple

-}

-- | Let's have a different type for aggregation evaluations and continuous ones

data Evaluation l i x = forall s . Evaluation
  { initial :: s
  , tracing :: forall r . Members (Output x ': State s ': l) r => i -> Sem r ()
  }

instance Category (Evaluation r) where
  id = Evaluation
    { initial = ()
    , tracing = output
    }
  (.) :: forall a b c l . Evaluation l b c -> Evaluation l a b -> Evaluation l a c
  (Evaluation i1 t1) . (Evaluation i2 t2) = go i2 i1 t2 t1 where
    go :: forall s s' . s -> s'
       -> (forall r . Members (Output b ': State s ': l) r => a -> Sem r ())
       -> (forall r . Members (Output c ': State s' ': l) r => b -> Sem r ())
       -> Evaluation l a c
    go s s' f g = Evaluation
      { initial = (s, s')
      , tracing = \a -> do
          undefined
          --interpret (\case
          --              Get -> undefined
          --              Put v -> undefined
          --          ) (interpret (\case
          --                           Output v -> undefined
          --                       ) (f a))


          --interpret
          --  \case Output b -> mapStateSecond @s @s' (g b)
          --  $ mapStateFirst @s @s'
          --  $ f a
      }
{-
instance Functor (Evaluation r i) where
  fmap :: forall a b . (a -> b) -> Evaluation i a -> Evaluation i b
  fmap f (Evaluation s t) = go s t where
    go :: forall s . s
       -> (forall r arr . (IArray arr (Maybe Int), Members '[Trace, Reader GraphWeights, State s] r) => arr Int (Maybe Int) -> i -> Sem (Output a ': r) ())
       -> Evaluation i b
    go initialState traceFun = Evaluation
      { initial = initialState
      , tracing = \arr event ->
          interpret
            \case Output o -> output (f o)
            $ traceFun arr event
      }

runEvaluation :: forall m r res arr i x a . (Members '[Lift m, Trace] r, Monoid res, MArray arr (Maybe Int) m) => GraphWeights -> arr Int (Maybe Int) -> Evaluation r i x -> (x -> res) -> Sem (Output i ': r) a -> Sem r (res, a)
runEvaluation weights tree Evaluation { .. } f = runFoldMapOutput f

  . fmap snd
  . runState initial
  . reinterpret2
    \case Output event -> runReader weights $ do
            withFrozen @m @Array tree $ \frozen -> 
              tracing frozen event

distanceTraveled :: Members '[Reader GraphWeights] r => Evaluation r ArvyEvent (Sum Double)
distanceTraveled = Evaluation () (\case
  RequestTravel a b _ -> do
    weights <- ask @GraphWeights
    output $ Sum $ weights ! (a, b)
  _ -> return ()
                                 )
-}
-- type Eval'' x = forall r a s . (Members '[Trace] r, Monoid s) => ArvyEvent -> Sem (State s ': Output x ': r (x, a)

--combineEvals :: Eval'' x -> Eval'' y -> Eval'' (x, y)
--combineEvals a b = undefined $ interpret \case
--  Output _ -> undefined

type Eval i o a = forall r b . Members '[Output i, Output o] r => Sem r b -> Sem r (a, b)

--data Eval' i o a = forall s . Eval'
--  { initialState :: s
--  , tracing :: forall r . Members '[Output o, State s] r => i -> Sem r ()
--  , getResult :: s -> a
--  }
--
--runEval :: forall i o a r x . Member (Output i) (Output o ': r) => Eval' i o a -> Sem r x -> Sem (Output o ': r) (a, x)
--runEval Eval' { .. } = fmap (first getResult)
--  . runState initialState
--  . intercept @(Output i)
--    \case Output i -> tracing i
--  . raise
--  . raise
--
--
--
--runEvalAggreg :: forall i a r x . Member (Output i) (Output Void ': r) => Eval' i Void a -> Sem r x -> Sem r (a, x)
--runEvalAggreg eval = runIgnoringOutput . runEval eval
--
--runEval' :: forall i o r x . Member (Output i) (Output o ': r) => Eval' i o () -> Sem r x -> Sem (Output o ': r) x
--runEval' eval = fmap snd <$> runEval eval
--
--testEval :: (Int, ())
--testEval = run $ runIgnoringOutput @Int $ runEvalAggreg sumEval x
--  where
--    x :: Member (Output Int) r => Sem r ()
--    x = do
--      output @Int 10
--      output @Int 120

{-
runEval' :: forall i o r x . Members '[Output i, Output o] r => Eval' i o () -> Sem r x -> Sem r x
runEval' eval = fmap snd <$> runEval eval

runEvalAggreg :: forall i a r x . Members '[Output i, Output Void] r => Eval' i Void a -> Sem r x -> Sem r (a, x)
runEvalAggreg eval s = runEval eval s
-}
type NooutEval i a = Eval i Void a

--sumEval :: Eval' Int Void Int
--sumEval = Eval'
--  { initialState = 0
--  , tracing = \v -> modify (+v)
--  , getResult = id
--  }
--
--requests :: Eval' ArvyEvent Int ()
--requests = Eval'
--  { initialState = ()
--  , tracing = \case
--      RequestMade i -> output i
--      _ -> return ()
--  , getResult = id
--  }

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
