{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Polysemy
import Polysemy.Random
import Arvy.Weights
import Arvy.Algorithm
import Data.Array hiding ((!))
import Data.Array.Unboxed ((!))
import Data.Array.IO
import Arvy.Requests
import Polysemy.Output
import Data.Monoid
import Polysemy.Trace
import Data.Time (getCurrentTime)
import Algebra.Graph.AdjacencyIntMap hiding (tree)
import System.Random (mkStdGen)

data WeightsParameter = WeightsParameter
  { weightsName :: String
  , weightsGet  :: forall r . Member Random r => Int -> Sem r GraphWeights
  }

data InitialTreeParameter = InitialTreeParameter
  { initialTreeName :: String
  , initialTreeGet  :: forall r . Member Random r => Int -> GraphWeights -> Sem r (Array Int (Maybe Int))
  }

instance Show InitialTreeParameter where
  show (InitialTreeParameter { .. }) = "Initial tree: " ++ initialTreeName ++ ", on an 8-ring: "
    ++ show (snd . run . runRandom (mkStdGen 0) $ initialTreeGet 8 (shortestPathWeights (symmetricClosure (circuit [0..7]))) )

data RequestsParameter = RequestsParameter
  { requestsName :: String
  , requestsGet  :: forall r . Member Random r => Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  }

data Parameters = Parameters
  { nodeCount    :: Int
  , weights      :: WeightsParameter
  , initialTree  :: InitialTreeParameter
  , requestCount :: Int
  , requests     :: RequestsParameter
  , algorithm    :: Arvy
  }

instance Show Parameters where
  show (Parameters { .. }) = "Parameters:\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tWeights: " ++ weightsName weights ++ "\n" ++
    "\tInitial tree: " ++ initialTreeName initialTree ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tRequests: " ++ requestsName requests ++ "\n" ++
    "\tAlgorithm: " ++ "TODO\n"

runParams :: Int -> Parameters -> IO ()
runParams seed params@Parameters
  { nodeCount
  , weights = WeightsParameter { weightsGet }
  , initialTree = InitialTreeParameter { initialTreeGet }
  , requestCount
  , requests = RequestsParameter { requestsGet }
  , algorithm = algorithm
  } = runM . fmap snd . runRandom (mkStdGen seed) . runTraceIO . timestampTraces $ do
  trace $ "Random seed: " ++ show seed
  trace $ show params

  -- TODO: Cache (and paralellize) these computations to make this faster

  trace $ "Generating weights.."
  !weights <- weightsGet nodeCount
  
  trace $ "Computing shortest paths.."
  let !shortestPaths = shortestPathWeights' weights
  
  trace $ "Generating initial tree.."
  !tree <- initialTreeGet nodeCount weights
  mutableTree <- sendM (thaw tree :: IO (IOArray Int (Maybe Int)))

  trace $ "Running arvy.."
  (Sum s, (n, _)) <- runFoldMapOutput Sum
    $ runIgnoringOutput
    $ measureRatio weights shortestPaths
    $ runRequests @IO mutableTree (requestsGet nodeCount weights) requestCount
    $ runArvyLocal @IO @IOArray weights mutableTree algorithm
  trace $ "Average (request path length) / (optimal path length): " ++ show (s / fromIntegral n)

timestampTraces :: Member (Lift IO) r => Sem (Trace ': r) a -> Sem (Trace ': r) a
timestampTraces = reinterpret \case
  Trace v -> do
    time <- sendM getCurrentTime
    trace $ "[" ++ show time ++ "] " ++ v

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
