{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Local
import Arvy.Algorithm
import Parameters.Tree
import Parameters.Requests
import Parameters.Weights
import Parameters.Algorithm

import Polysemy
import Polysemy.RandomFu
import qualified Data.Vector as V
import GHC.Word
import Data.Array.IO
import Polysemy.Trace
import System.Random.MWC
import Utils
import Cache
import Conduit


data Parameters r = Parameters
  { randomSeed   :: Word32
  , nodeCount    :: Int
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , algorithm    :: AlgorithmParameter r
  , requests     :: RequestsParameter r
  }

instance Show (Parameters r) where
  show Parameters { algorithm = AlgorithmParameter { algorithmDescription }, .. } = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ weightsDescription weights ++ "\n" ++
    "\tAlgorithm: " ++ algorithmDescription ++ "\n" ++
    "\tRequests: " ++ requestsDescription requests ++ "\n"

data ConstParameters = ConstParameters
  { paramNodeCount :: NodeCount
  , paramWeights :: GraphWeights
  , paramTree :: IOUArray Node Node
  }

paramFile :: Parameters r -> String -> FilePath
paramFile params metric = "weights:" ++ weightsId (weights params) ++ "/requests:" ++ requestsId (requests params) ++ "/metric:" ++ metric ++ "/algorithm:" ++ algorithmId (algorithm params)

runRandomSeed :: Members '[Lift IO] r => Word32 -> Sem (RandomFu ': r) a -> Sem r a
runRandomSeed seed sem = do
  gen <- sendM $ initialize (V.singleton seed)
  runRandomSource' gen sem

data Env = Env
  { envNodeCount :: NodeCount
  , envWeights :: GraphWeights
  , envTree :: IOUArray Node Node
  }

runParams
  :: forall r x
  . Members '[Lift IO, Trace] r
  => Parameters (RandomFu ': r)
  -> (Env -> ConduitT ArvyEvent Void (Sem (RandomFu ': r)) x)
  -> Sem r x
runParams params@Parameters
  { randomSeed = seed
  , nodeCount
  , weights = WeightsParameter { weightsGet, weightsId }
  , requestCount
  , requests = RequestsParameter { requestsGet }
  , algorithm = AlgorithmParameter { algorithmGet, algorithmInitialTree }
  } evaluation = do
  trace $ show params
  generatedParams <- genParams algorithmInitialTree
  runAlg generatedParams algorithmGet
  --transPipe (runRandomSource' gen) $ runAlg generatedParams algorithmGet


  where

    genParams :: forall s . InitialTreeParameter s (RandomFu ': r) -> Sem r (ConstParameters, IOArray Node s)
    genParams InitialTreeParameter { initialTreeGet } = do
      trace "Generating weights.."
      !weights <- cache (CacheKey ("weights-" ++ weightsId ++ "-" ++ show nodeCount ++ "-" ++ show seed)) (runRandomSeed seed $ weightsGet nodeCount)

      trace "Generating initial tree.."
      (tree, states) <- runRandomSeed seed $ initialTreeGet nodeCount weights
      mutableTree <- sendM (thaw tree :: IO (IOUArray Int Int))
      mutableStates <- sendM (thaw states :: IO (IOArray Int s))

      return (ConstParameters nodeCount weights mutableTree, mutableStates)

    runAlg :: forall s . (ConstParameters, IOArray Node s) -> Arvy s (RandomFu ': r) -> Sem r x
    runAlg (ConstParameters { .. }, states) algorithm = do
      reqs <- runRandomSeed seed $ requestsGet nodeCount paramWeights

      trace "Running arvy.."
      let env = Env nodeCount paramWeights paramTree
      runRandomSeed seed $ runConduit $ runRequests paramTree reqs requestCount
        .| runArvyLocal paramWeights paramTree states algorithm
        .| evaluation env
