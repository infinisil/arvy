{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Algorithm
import Arvy.Local
import Parameters.Tree
import Parameters.Requests
import Parameters.Weights
import Parameters.Algorithm
import Utils
import Cache
import Arvy.Log
import Data.MonoTraversable
import qualified Data.Sequence as S
import Data.NonNull hiding (last)

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
import qualified Data.Conduit.Combinators as C
import Arvy.Weight
import Data.List

data SharedParams r = SharedParams
  { sharedParamRandomSeed :: Word32
  , sharedParamRequestCount :: Int
  , sharedParamRequests :: RequestsParameter r
  , sharedParamEvals :: [Eval r]
  }

data GenParams a r = GenParams
  { genParamShared :: SharedParams r
  , genParamInit :: ArvyData a
  , genParamAlgs :: [GenAlgParam a r]
  }

evalsConduit
  :: ( Traversable f
     , Member Trace r )
  => String
  -> f (Eval r)
  -> Env
  -> ConduitT (NonNull (S.Seq Node)) Void (Sem r) (f Series)
evalsConduit prefix evals env = --C.iterM (trace . show) .|
  sequenceConduits (fmap (\eval -> do
                             vals <- evalFun eval env .| C.sinkList
                             lift $ trace $ prefix ++ ", " ++ evalName eval ++ ": " ++ show (last vals)
                             return vals
                         ) evals)

getWeightsArray :: HasWeights a => ArvyData a -> GraphWeights
getWeightsArray = undefined

runGenParams
  :: forall r a
   . ( LogMember r
     , Members '[Lift IO, Trace] r
     , HasSuccessor a
     , HasWeights a )
  => GenParams a r
  -> Sem r EvalResults
runGenParams GenParams { genParamShared = SharedParams { .. }, .. } = do
  series <- transpose <$> mapM runAlg genParamAlgs
  return $ EvalResults "TODO" $ zip
    (map evalName sharedParamEvals)
    (map (zip (map genAlgName genParamAlgs)) series)
  where
    runAlg :: GenAlgParam a r -> Sem r [Series]
    runAlg (GenAlgParam name (GeneralArvy spec)) = do
      (mutableTree, conduit) <- runArvySpecLocal' @(S.Seq Int) genParamInit spec
      let env = Env
            { envNodeCount = arvyDataNodeCount genParamInit
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray genParamInit
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- requestsGet sharedParamRequests env
      runConduit $ C.replicateM sharedParamRequestCount request
        .| C.mapM (\node -> (node `ncons`) <$> conduit node) .| evals





data SpecParams r = forall p a . SpecParams
  { specParamShared :: SharedParams r
  , specParamInit :: p
  , specParamAlg :: SpecAlgParam p a r
  , specParamGenAlgs :: [GenAlgParam a r]
  }


data Parameters p r = Parameters
  { randomSeed   :: Word32
  , initializer  :: p
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , requests     :: RequestsParameter r
  } deriving (Eq, Ord)

paramDescr :: Show p => Parameters p r -> String
paramDescr Parameters { .. } = "weights: " ++ weightsId weights ++ ", initializer: " ++ show initializer ++ ", requests: " ++ requestsId requests

instance Show p => Show (Parameters p r) where
  show Parameters { .. } = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tinitializer: " ++ show initializer ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ show weights ++ "\n" ++
    "\tRequests: " ++ show requests ++ "\n"



genWeights :: Members '[Trace, Lift IO] r => Parameters p (RandomFu ': r) -> ArvyData a -> Sem r GraphWeights
genWeights Parameters { randomSeed = seed, initializer, weights = WeightsParameter { weightsId, weightsGet } } arvyData = do
  trace "Generating weights.."
  let nodeCount = arvyDataNodeCount arvyData

  !result <- cache (CacheKey ("weights-" ++ weightsId ++ "-" ++ show nodeCount ++ "-" ++ show seed)) (runRandomSeed seed $ weightsGet nodeCount)
  return result


-- | Generate the final parameter values, caching the weights during that
--genParams :: forall r s p a . Members '[Trace, Lift IO] r => Parameters p (RandomFu ': r) -> AlgorithmParameter p a r -> Sem r (Env, IOArray Node s)
--genParams params@Parameters { randomSeed = seed, initializer, requestCount } AlgorithmParameter { algorithmGet }= do
--  arvyData <- getArvyData algorithmGet
--  weights <- genWeights params arvyData
--  let nodeCount = arvyDataNodeCount arvyData
--
--  trace "Generating initial tree.."
--  (tree, states) <- runRandomSeed seed $ initialTreeGet nodeCount weights
--  mutableTree <- sendM (thaw tree)
--  mutableStates <- sendM (thaw states)
--
--  return (Env nodeCount requestCount weights mutableTree, mutableStates)

-- | Run parameters on an algorithm while doing an evaluation on the resulting events, returning the evaluations result
--runParams
--  :: ( Members '[Lift IO, Trace, PA.Async, Log] r
--     , NFData x)
--  => Parameters p (RandomFu ': r)
--  -> AlgorithmParameter p a (RandomFu ': r)
--  -> (Env -> ConduitT (NonNull (S.Seq Node)) Void (Sem (RandomFu ': r)) x)
--  -> Sem r (Async (Maybe x))
--runParams params@Parameters
--  { randomSeed = seed
--  , requestCount
--  , initializer
--  , requests = RequestsParameter { requestsGet }
--  }
--  alg@AlgorithmParameter { algorithmGet }
--  evaluation = do
--
--  --trace $ show params
--  trace $ show alg
--
--  ((mutableTree, mutableStates), conduit) <- runRandomSeed seed $ runArvyLocal' initializer algorithmGet
--
--  let env = Env nodeCount requestCount weights mutableTree
--
--  runRandomSeed seed $ runConduit
--    $ C.replicateM requestCount reqs
--    .| conduit
--    .| traceRequests
--    .| evaluation env
--
--
--  undefined

  --(env@Env { .. }, states) <- genParams params algorithmInitialTree


  --reqs <- runRandomSeed seed $ requestsGet env

  --PA.async $ do

  --  trace "Running arvy.."
  --  runRandomSeed seed $ runConduit $
  --      C.replicateM requestCount reqs
  --      .| runArvyLocal envWeights envTree states algorithmGet
  --      .| traceRequests
  --      .| evaluation env
