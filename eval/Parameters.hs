{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parameters where

import           Arvy.Algorithm
import           Arvy.Local
import           Arvy.Log
import           Data.NonNull             hiding (last)
import qualified Data.Sequence            as S
import qualified Data.Text                as T
import           Parameters.Algorithm
import           Parameters.Requests
import           Parameters.Tree
import           Parameters.Weights

import           Conduit                  hiding (await)
import qualified Control.Concurrent.Async as A
import           Data.Array.Unboxed
import qualified Data.Conduit.Combinators as C
import           Data.List
import           Data.Maybe               (fromJust)
import           Evaluation.Request
import           Evaluation.Types
import           GHC.Word
import           Polysemy
import           Polysemy.Async
import           Polysemy.RandomFu
import           Utils

-- | Shared parameters between general and specific parameters
data SharedParams r = SharedParams
  { sharedParamRandomSeed   :: Word32
  -- ^ The random seed to use for all randomness
  , sharedParamRequestCount :: Int
  -- ^ How many requests to issue
  , sharedParamRequests     :: RequestsParameter r
  -- ^ What kind of requests to issue
  , sharedParamEvals        :: [Eval r]
  -- ^ What evaluations/statistics to collect
  }

-- | Parameters specific to general algorithms
data GenParams r = GenParams
  { genParamShared    :: SharedParams r
  -- ^ The shared parameters
  , genParamNodeCount :: NodeCount
  -- ^ How many nodes to use
  , genParamWeights   :: WeightsParam r
  -- ^ What weights to use between nodes
  , genParamAlgs      :: [(GenAlgParam r, TreeParam r)]
  -- ^ What set of general algorithms with which initial trees to compare
  }

-- | Collects multiple evaluations into a single one
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

-- | Evaluates a 'GenParams'
{-# INLINE runGenParams #-}
runGenParams
  :: forall r
   . ( LogMember r
     , Members '[Lift IO, Async] r )
  => GenParams (RandomFu ': r)
  -> Sem r EvalResults
runGenParams GenParams { genParamShared = SharedParams { .. }, .. } = do
  weights <- runRand 0 $ weightsGen genParamWeights genParamNodeCount
  asyncs <- mapM (runAlg weights) genParamAlgs
  series <- transpose <$> mapM await asyncs
  return $ EvalResults "TODO" $ zip
    (map evalName sharedParamEvals)
    (map (zip (map (\(alg, tree) -> genAlgName alg <> "-" <> treeName tree) genParamAlgs)) series)
  where
    {-# INLINE runAlg #-}
    runAlg :: GraphWeights -> (GenAlgParam (RandomFu ': r), TreeParam (RandomFu ': r)) -> Sem r (A.Async [Series])
    runAlg weights (GenAlgParam name (GeneralArvy spec), TreeParam { .. }) = do
      tree <- runRand 1 $ treeGen genParamNodeCount weights
      lgDebug $ "Tree is " <> tshow tree
      let arvyData = ArvyData
            { arvyDataNodeCount = genParamNodeCount
            , arvyDataNodeData = \node -> ArvyNodeData
              { arvyNodeSuccessor = tree ! node
              , arvyNodeWeights = \other -> weights ! (node, other)
              , arvyNodeAdditional = ()
              }
            }

      (mutableTree, conduit) <- runRand 2 $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount arvyData
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand 3 $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand 4
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals

    {-# INLINE runRand #-}
    runRand :: Word32 -> Sem (RandomFu ': r) x -> Sem r x
    runRand shift = runRandomSeed (sharedParamRandomSeed + shift)





-- | Parameters for testing a heuristics specialized to certain graphs
data SpecParams p a r = SpecParams
  { specParamShared  :: SharedParams r
  -- ^ The shared parameters
  , specParamInit    :: p
  -- ^ What data to initialize the specialized heuristic with
  , specParamAlg     :: SpecAlgParam p a r
  -- ^ What specialized heuristic to use
  , specParamGenAlgs :: [(GenAlgParam r, TreeParam r)]
  -- ^ What general heuristics along with initial trees to compare the specialized algorithm to
  }

-- | Evaluates a 'SpecParams'
{-# INLINE runSpecParams #-}
runSpecParams
  :: forall p a r
   . ( LogMember r
     , Members '[Lift IO, Async] r )
  => SpecParams p a (RandomFu ': r)
  -> Sem r EvalResults
runSpecParams SpecParams { specParamShared = SharedParams { .. }, specParamAlg = specAlg@SpecAlgParam { specAlg = SpecializedArvy generator _, .. }, .. } = do

  -- Generate the algorithm-specific arvy data
  specArvyData <- runRand 0 $ generator specParamInit

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

      (mutableTree, conduit) <- runRand 2 $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand 3 $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand 4
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals
    {-# INLINE runAlg #-}
    runAlg :: ArvyData a -> (GenAlgParam (RandomFu ': r), TreeParam (RandomFu ': r)) -> Sem r (A.Async [Series])
    runAlg dat@ArvyData { .. } (GenAlgParam name (GeneralArvy spec), TreeParam { .. }) = do
      let weights = getWeightsArray dat
      tree <- runRand 1 $ treeGen arvyDataNodeCount weights
      let arvyData = ArvyData
            { arvyDataNodeCount = arvyDataNodeCount
            , arvyDataNodeData = \node -> ArvyNodeData
              { arvyNodeSuccessor = tree ! node
              , arvyNodeWeights = \other -> weights ! (node, other)
              , arvyNodeAdditional = ()
              }
            }

      (mutableTree, conduit) <- runRand 2 $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount
            , envRequestCount = sharedParamRequestCount
            , envWeights = weights
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand 3 $ requestsGet sharedParamRequests env
      fmap (fmap fromJust)
        $ async
        $ runRand 4
        $ runConduit
        $ C.replicateM sharedParamRequestCount request
          .| C.mapM (\node -> (node `ncons`) <$> conduit node)
          .| traceRequests
          .| evals

    {-# INLINE runRand #-}
    runRand :: Word32 -> Sem (RandomFu ': r) x -> Sem r x
    runRand shift = runRandomSeed (sharedParamRandomSeed + shift)
