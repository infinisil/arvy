{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Local
import Parameters.Tree
import Parameters.Requests
import Parameters.Weights
import Parameters.Algorithm
import Utils
import Cache

import Polysemy
import Polysemy.RandomFu
import GHC.Word
import Data.Array.IO
import Polysemy.Trace
import Conduit
import Evaluation.Types
import Evaluation.Request
import qualified Polysemy.Async as PA
import Control.Concurrent.Async
import Control.DeepSeq


data Parameters r = Parameters
  { randomSeed   :: Word32
  , nodeCount    :: Int
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , requests     :: RequestsParameter r
  } deriving (Eq, Ord)

paramDescr :: Parameters r -> String
paramDescr Parameters { .. } = "weights: " ++ weightsId weights ++ ", node count: " ++ show nodeCount ++ ", requests: " ++ requestsId requests

instance Show (Parameters r) where
  show Parameters { .. } = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ show weights ++ "\n" ++
    "\tRequests: " ++ show requests ++ "\n"

genWeights :: Members '[Trace, Lift IO] r => Parameters (RandomFu ': r) -> Sem r GraphWeights
genWeights Parameters { randomSeed = seed, nodeCount, weights = WeightsParameter { weightsId, weightsGet } } = do
  trace "Generating weights.."
  !result <- cache (CacheKey ("weights-" ++ weightsId ++ "-" ++ show nodeCount ++ "-" ++ show seed)) (runRandomSeed seed $ weightsGet nodeCount)
  return result

-- | Generate the final parameter values, caching the weights during that
genParams :: forall r s . Members '[Trace, Lift IO] r => Parameters (RandomFu ': r) -> GraphWeights -> InitialTreeParameter s (RandomFu ': r) -> Sem r (Env, IOArray Node s)
genParams Parameters { randomSeed = seed, nodeCount, requestCount } weights InitialTreeParameter { initialTreeGet } = do

  trace "Generating initial tree.."
  (tree, states) <- runRandomSeed seed $ initialTreeGet nodeCount weights
  mutableTree <- sendM (thaw tree)
  mutableStates <- sendM (thaw states)

  return (Env nodeCount requestCount weights mutableTree, mutableStates)

-- | Run parameters on an algorithm while doing an evaluation on the resulting events, returning the evaluations result
runParams
  :: ( Members '[Lift IO, Trace, PA.Async] r
     , NFData x )
  => Parameters (RandomFu ': r)
  -> AlgorithmParameter (RandomFu ': r)
  -> (Env -> ConduitT ArvyEvent Void (Sem (RandomFu ': r)) x)
  -> Sem r (Async (Maybe x))
runParams params@Parameters
  { randomSeed = seed
  , requestCount
  , requests = RequestsParameter { requestsGet }
  }
  alg@AlgorithmParameter { algorithmGet, algorithmInitialTree }
  evaluation = do

  trace $ show params
  trace $ show alg

  weights <- genWeights params

  PA.async $ do
    (env@Env { .. }, states) <- genParams params weights algorithmInitialTree

    reqs <- runRandomSeed seed $ requestsGet env

    trace "Running arvy.."
    runRandomSeed seed $ runConduit $
        runRequests reqs requestCount
        .| runArvyLocal envWeights envTree states algorithmGet
        .| traceRequests
        .| evaluation env
