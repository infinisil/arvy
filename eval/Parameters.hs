{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Time (getCurrentTime)
import System.Random.MWC
import Utils
import Pipes
import Data.Time
import Cache
import Polysemy.Async
import qualified Control.Concurrent.Async as A


data Parameters r = Parameters
  { randomSeed   :: Word32
  , nodeCount    :: Int
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , algorithm    :: AlgorithmParameter r
  , requests     :: RequestsParameter r
  }

instance Show (Parameters r) where
  show (Parameters { algorithm = AlgorithmParameter { algorithmDescription }, .. }) = "Parameters:\n" ++
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

runParams
  :: forall r
  . Members '[Async, Lift IO, Trace] r
  => Parameters (RandomFu ': r)
  -> (NodeCount -> GraphWeights -> IOUArray Node Node -> Consumer ArvyEvent (Sem (RandomFu ': r)) ())
  -> Sem r (A.Async (Maybe ()))
runParams params@Parameters
  { randomSeed = seed
  , nodeCount
  , weights = WeightsParameter { weightsGet, weightsId }
  , requestCount
  , requests = RequestsParameter { requestsGet }
  , algorithm = AlgorithmParameter { algorithmGet, algorithmInitialTree }
  } evaluation = do
  trace $ show params
  gen <- sendM $ initialize (V.singleton seed)
  runRandomSource' gen $ do
    generatedParams <- genParams algorithmInitialTree
    runAlg generatedParams algorithmGet

  where

    genParams :: forall s . InitialTreeParameter s (RandomFu ': r) -> Sem (RandomFu ': r) (ConstParameters, IOArray Node s)
    genParams InitialTreeParameter { initialTreeGet } = do
      trace "Generating weights.."
      !weights <- cache (CacheKey ("weights-" ++ weightsId ++ "-" ++ show nodeCount ++ "-" ++ show seed)) (weightsGet nodeCount)

      trace "Generating initial tree.."
      !(tree, states) <- initialTreeGet nodeCount weights
      mutableTree <- sendM (thaw tree :: IO (IOUArray Int Int))
      mutableStates <- sendM (thaw states :: IO (IOArray Int s))

      return (ConstParameters nodeCount weights mutableTree, mutableStates)

    runAlg :: forall s . (ConstParameters, IOArray Node s) -> Arvy s (RandomFu ': r) -> Sem (RandomFu ': r) (A.Async (Maybe ()))
    runAlg (ConstParameters { .. }, states) algorithm = async do
      reqs <- requestsGet nodeCount paramWeights

      trace $ "Running arvy.."
      start <- sendM getCurrentTime
      runEffect $ runRequests paramTree reqs requestCount
        >-> runArvyLocal paramWeights paramTree states algorithm
        >-> evaluation nodeCount paramWeights paramTree
      end <- sendM getCurrentTime
      trace $ "Took " ++ show (end `diffUTCTime` start)
