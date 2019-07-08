{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Local
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
  show (Parameters { .. }) = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ weightsName weights ++ "\n" ++
    "\tAlgorithm: " ++ algorithmName algorithm ++ "\n" ++
    "\tInitial tree: " ++ initialTreeName (algorithmInitialTree algorithm) ++ "\n" ++
    "\tRequests: " ++ requestsName requests ++ "\n"

runParams :: (Members '[Lift IO, Trace, Output res] r) => Parameters (RandomFu ': r) -> (Int -> GraphWeights -> IOUArray Int Int -> Eval ArvyEvent res) -> Sem r ()
runParams params@Parameters
  { randomSeed = seed
  , nodeCount
  , weights = WeightsParameter { weightsGet }
  , requestCount
  , requests = RequestsParameter { requestsGet }
  , algorithm = AlgorithmParameter { algorithmGet, algorithmInitialTree = InitialTreeParameter { initialTreeGet } }
  } evaluation = do
  gen <- sendM $ initialize (V.singleton seed)
  runRandomSource' gen $ do
    trace $ "Random seed: " ++ show seed
    trace $ show params

    let algorithm = algorithmGet nodeCount

    -- TODO: Cache (and paralellize) these computations to make this faster

    trace $ "Generating weights.."
    !weights <- weightsGet nodeCount

    trace $ "Generating initial tree.."
    !tree <- initialTreeGet nodeCount weights
    mutableTree <- sendM (thaw tree :: IO (IOUArray Int Int))

    let eval = evaluation nodeCount weights mutableTree

    reqs <- requestsGet nodeCount weights

    trace $ "Running arvy.."
    runEval eval
      $ runRequests @IO mutableTree (raise . reqs) requestCount
      $ runArvyLocal @IO @IOArray nodeCount weights mutableTree algorithm

timestampTraces :: Members '[Lift IO, Trace] r => Sem (Trace ': r) a -> Sem r a
timestampTraces = interpret \case
  Trace v -> do
    time <- sendM getCurrentTime
    trace $ "[" ++ show time ++ "] " ++ v

--measureRatio :: Member Trace r => GraphWeights -> GraphWeights -> Sem (Output ArvyEvent ': r) a -> Sem (Output ArvyEvent ': Output Double ': r) (Int, a)
--measureRatio weights shortestPaths = fmap (\((_, n), a) -> (n, a)) . runState (0.0 :: Double, 0) . reinterpret3 \case
--  Output event -> do
--    output event
--    case event of
--      RequestMade _ -> do
--        modify @(Double, Int) $ \(_, n) -> (0.0, n)
--      RequestTravel a b _ -> do
--        modify @(Double, Int) $ \(d, n) -> (d + weights ! (a, b), n)
--      RequestGranted (GottenFrom i src) -> do
--        (pathLength, _) <- get @(Double, Int)
--        modify @(Double, Int) $ \(d, n) -> (d, n + 1)
--        output $ pathLength / shortestPaths ! (i, src)
--      _ -> return ()
