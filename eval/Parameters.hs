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
import Evaluation

import Polysemy
import Polysemy.RandomFu
import GHC.Word
import Data.Array.IO
import Polysemy.Trace
import Conduit
import Graphics.Rendering.Chart
import Data.List


data Parameters r = Parameters
  { randomSeed   :: Word32
  , nodeCount    :: Int
  , requestCount :: Int
  , weights      :: WeightsParameter r
  , requests     :: RequestsParameter r
  } deriving (Eq, Ord)

instance Show (Parameters r) where
  show Parameters { .. } = "Parameters:\n" ++
    "\tRandom seed: " ++ show randomSeed ++ "\n" ++
    "\tNode count: " ++ show nodeCount ++ "\n" ++
    "\tRequest count: " ++ show requestCount ++ "\n" ++
    "\tWeights: " ++ show weights ++ "\n" ++
    "\tRequests: " ++ show requests ++ "\n"

paramFile :: Parameters r -> String -> FilePath
paramFile params metric = "weights:" ++ weightsId (weights params) ++ "/requests:" ++ requestsId (requests params) ++ "/metric:" ++ metric



runEvals
  :: forall r
   . Members '[Lift IO, Trace] r
  => Parameters (RandomFu ': r)
  -> [AlgorithmParameter (RandomFu ': r)]
  -> [Eval (Sem (RandomFu ': r))]
  -> Sem r [Layout Double Double]
runEvals params algs evals = do
  series <- transpose <$> mapM runit algs
  return $ zipWith toLayout (map evalPlotDefaults evals) series
  where
  runit :: AlgorithmParameter (RandomFu ': r) -> Sem r [Series]
  runit alg = runParams params alg (evalsConduit evals)

  toLayout :: PlotDefaults -> [Series] -> Layout Double Double
  toLayout PlotDefaults { .. } series = plotDefaultLayout { _layout_plots = zipWith mkPlot algs series }
    where mkPlot alg serie = toPlot (plotDefaultPlot alg) { _plot_lines_values = [serie] }


-- | Generate the final parameter values, caching the weights during that
genParams :: forall r s . Members '[Trace, Lift IO] r => Parameters (RandomFu ': r) -> InitialTreeParameter s (RandomFu ': r) -> Sem r (Env, IOArray Node s)
genParams Parameters { randomSeed = seed, nodeCount, weights = WeightsParameter { weightsId, weightsGet } } InitialTreeParameter { initialTreeGet } = do
  trace "Generating weights.."
  !weights <- cache (CacheKey ("weights-" ++ weightsId ++ "-" ++ show nodeCount ++ "-" ++ show seed)) (runRandomSeed seed $ weightsGet nodeCount)

  trace "Generating initial tree.."
  (tree, states) <- runRandomSeed seed $ initialTreeGet nodeCount weights
  mutableTree <- sendM (thaw tree)
  mutableStates <- sendM (thaw states)

  return (Env nodeCount weights mutableTree, mutableStates)

-- | Run parameters on an algorithm while doing an evaluation on the resulting events, returning the evaluations result
runParams
  :: Members '[Lift IO, Trace] r
  => Parameters (RandomFu ': r)
  -> AlgorithmParameter (RandomFu ': r)
  -> (Env -> ConduitT ArvyEvent Void (Sem (RandomFu ': r)) x)
  -> Sem r x
runParams params@Parameters
  { randomSeed = seed
  , nodeCount
  , requestCount
  , requests = RequestsParameter { requestsGet }
  }
  AlgorithmParameter { algorithmGet, algorithmInitialTree }
  evaluation = do

  trace $ show params

  (env@Env { .. }, states) <- genParams params algorithmInitialTree

  reqs <- runRandomSeed seed $ requestsGet nodeCount envWeights

  trace "Running arvy.."
  runRandomSeed seed $ runConduit $
    runRequests envTree reqs requestCount
    .| runArvyLocal envWeights envTree states algorithmGet
    .| evaluation env
