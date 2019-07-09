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
import Evaluation

import Polysemy
import Polysemy.RandomFu
import qualified Data.Vector as V
import GHC.Word
import Data.Array.IO
import Polysemy.Output
import Polysemy.Trace
import Data.Time (getCurrentTime)
import System.Random.MWC
import Utils


data Parameters r = Parameters
  { randomSeed   :: Word32
  , nodeCount    :: Int
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , algorithm    :: AlgorithmParameter r
  , requests     :: RequestsParameter r
  }

instance Show (Parameters r) where
  show (Parameters { algorithm = AlgorithmParameter { algorithmName, algorithmInitialTree }, .. }) = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ weightsName weights ++ "\n" ++
    "\tAlgorithm: " ++ algorithmName ++ "\n" ++
    "\tInitial tree: " ++ initialTreeName algorithmInitialTree ++ "\n" ++
    "\tRequests: " ++ requestsName requests ++ "\n"

runParams :: forall res r . (Members '[Lift IO, Trace, Output res] r) => Parameters (RandomFu ': r) -> (Int -> GraphWeights -> IOUArray Int Int -> Eval ArvyEvent res) -> Sem r ()
runParams params@Parameters
  { randomSeed = seed
  , nodeCount
  , weights = WeightsParameter { weightsGet }
  , requestCount
  , requests = RequestsParameter { requestsGet }
  , algorithm = AlgorithmParameter { algorithmGet, algorithmInitialTree }
  } evaluation = do
  gen <- sendM $ initialize (V.singleton seed)
  runRandomSource' gen
    $ go algorithmInitialTree algorithmGet

  where
  go :: forall s . InitialTreeParameter s (RandomFu ': r) -> Arvy s (RandomFu ': r) -> Sem (RandomFu ': r) ()
  go InitialTreeParameter { initialTreeGet } algorithm = do
    trace $ show params
    
    -- TODO: Cache (and paralellize) these computations to make this faster

    trace $ "Generating weights.."
    !weights <- weightsGet nodeCount
    

    trace $ "Generating initial tree.."
    !(tree, states) <- initialTreeGet nodeCount weights
    mutableTree <- sendM (thaw tree :: IO (IOUArray Int Int))
    mutableStates <- sendM (thaw states :: IO (IOArray Int s))

    let eval = evaluation nodeCount weights mutableTree
    reqs <- requestsGet nodeCount weights

    trace $ "Running arvy.."
    runEval eval
      $ runRequests @IO mutableTree (raise . reqs) requestCount
      $ runArvyLocal @IO @s @IOArray weights mutableTree mutableStates algorithm

timestampTraces :: Members '[Lift IO, Trace] r => Sem (Trace ': r) a -> Sem r a
timestampTraces = interpret \case
  Trace v -> do
    time <- sendM getCurrentTime
    trace $ "[" ++ show time ++ "] " ++ v
