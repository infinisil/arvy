{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parameters where

import Arvy.Algorithm
import Arvy.Local
import Parameters.Requests
import Parameters.Algorithm
import Parameters.Weights
import Parameters.Tree
import Arvy.Log
import qualified Data.Sequence as S
import Data.NonNull hiding (last)
import qualified Data.Text as T
import Data.Text (Text)

import Polysemy
import GHC.Word
import Conduit hiding (await)
import Evaluation.Request
import Evaluation.Types
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Array.Unboxed
import Polysemy.RandomFu
import Utils
import Polysemy.Async
import qualified Control.Concurrent.Async as A
import Data.Maybe (fromJust)
import Control.Monad

data SharedParams r = SharedParams
  { sharedParamRandomSeed :: Word32
  , sharedParamRequestCount :: Int
  , sharedParamRequests :: RequestsParameter r
  , sharedParamEvals :: [Eval r]
  }

data GenParams r = GenParams
  { genParamShared :: SharedParams r
  , genParamNodeCount :: NodeCount
  , genParamWeights :: WeightsParam r
  , genParamAlgs :: [(GenAlgParam r, TreeParam r)]
  }

evalsConduit
  :: forall r f
   . ( Traversable f
     , LogMember r )
  => T.Text
  -> f (Eval r)
  -> Env
  -> ConduitT (NonNull (S.Seq Node)) Void (Sem r) (f Series)
evalsConduit prefix evals env = sequenceConduits (fmap runEval evals) where
  runEval eval = do
    vals <- evalFun eval env .| C.sinkList
    lift $ lgInfo $ prefix <> ", " <> evalName eval <> ": " <> tshow (last vals)
    return vals

getWeightsArray :: ArvyData a -> GraphWeights
getWeightsArray ArvyData { .. } = listArray ((0, 0), (arvyDataNodeCount - 1, arvyDataNodeCount - 1)) weights where
  weights = [ arvyNodeWeights (arvyDataNodeData u) v
            | u <- [0..arvyDataNodeCount - 1]
            , v <- [0..arvyDataNodeCount - 1]
            ]

{-# INLINE runGenParams #-}
runGenParams
  :: forall r
   . ( LogMember r
     , Members '[Lift IO, Async] r )
  => GenParams (RandomFu ': r)
  -> Sem r EvalResults
runGenParams GenParams { genParamShared = SharedParams { .. }, .. } = do
  weights <- runRand $ weightsGen genParamWeights genParamNodeCount
  asyncs <- mapM (runAlg weights) genParamAlgs
  series <- transpose <$> mapM await asyncs
  return $ EvalResults "TODO" $ zip
    (map evalName sharedParamEvals)
    (map (zip (map (\(alg, tree) -> genAlgName alg <> "-" <> treeName tree) genParamAlgs)) series)
  where
    {-# INLINE runAlg #-}
    runAlg :: GraphWeights -> (GenAlgParam (RandomFu ': r), TreeParam (RandomFu ': r)) -> Sem r (A.Async [Series])
    runAlg weights (GenAlgParam name (GeneralArvy spec), TreeParam { .. }) = do
      tree <- runRand $ treeGen genParamNodeCount weights
      let arvyData = ArvyData
            { arvyDataNodeCount = genParamNodeCount
            , arvyDataNodeData = \node -> ArvyNodeData
              { arvyNodeSuccessor = tree ! node
              , arvyNodeWeights = \other -> weights ! (node, other)
              , arvyNodeAdditional = ()
              }
            }

      (mutableTree, conduit) <- runRand $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount arvyData
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals

    {-# INLINE runRand #-}
    runRand :: Sem (RandomFu ': r) x -> Sem r x
    runRand = runRandomSeed sharedParamRandomSeed





data SpecParams p a r = SpecParams
  { specParamShared :: SharedParams r
  , specParamInit :: p
  , specParamAlg :: SpecAlgParam p a r
  , specParamGenAlgs :: [(GenAlgParam r, TreeParam r)]
  }

{-# INLINE runSpecParams #-}
runSpecParams
  :: forall p a r
   . ( LogMember r
     , Members '[Lift IO, Async] r )
  => SpecParams p a (RandomFu ': r)
  -> Sem r EvalResults
runSpecParams SpecParams { specParamShared = SharedParams { .. }, specParamAlg = specAlg@SpecAlgParam { specAlg = SpecializedArvy generator _, .. }, .. } = do

  -- Generate the algorithm-specific arvy data
  specArvyData <- runRand $ generator specParamInit

  -- Run the specialized algorithm
  specAsync <- runAlg' specArvyData specAlg

  -- Run the generalized algorithms on the same arvy data (minus the tree)
  genAsyncs <- mapM (runAlg specArvyData) specParamGenAlgs

  -- Wait for the results, then transpose from "algorithm -> evaluation -> series"
  -- to "evaluation -> algorithm -> series"
  series <- transpose <$> mapM await (specAsync : genAsyncs)
  --let series = transpose (specAsync : genAsyncs)
  return $ EvalResults "TODO" $ zip
    -- The names of the evals
    (map evalName sharedParamEvals)
    (map (zip (specAlgName : map (\(alg, tree) -> genAlgName alg <> "-" <> treeName tree) specParamGenAlgs)) series)
  where

    {-# INLINE runAlg' #-}
    runAlg' :: ArvyData a -> SpecAlgParam p a (RandomFu ': r) -> Sem r (A.Async [Series])
    runAlg' arvyData@ArvyData { .. } (SpecAlgParam name (SpecializedArvy _ spec)) = do

      (mutableTree, conduit) <- runRand $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals
    {-# INLINE runAlg #-}
    runAlg :: ArvyData a -> (GenAlgParam (RandomFu ': r), TreeParam (RandomFu ': r)) -> Sem r (A.Async [Series])
    runAlg dat@ArvyData { .. } (GenAlgParam name (GeneralArvy spec), TreeParam { .. }) = do
      let weights = getWeightsArray dat
      tree <- runRand $ treeGen arvyDataNodeCount weights
      let arvyData = ArvyData
            { arvyDataNodeCount = arvyDataNodeCount
            , arvyDataNodeData = \node -> ArvyNodeData
              { arvyNodeSuccessor = tree ! node
              , arvyNodeWeights = \other -> weights ! (node, other)
              , arvyNodeAdditional = ()
              }
            }

      (mutableTree, conduit) <- runRand $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount
            , envRequestCount = sharedParamRequestCount
            , envWeights = weights
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals

    {-# INLINE runRand #-}
    runRand :: Sem (RandomFu ': r) x -> Sem r x
    runRand = runRandomSeed sharedParamRandomSeed
