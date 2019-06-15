module Arvy where

import           Data.Array.IArray
import           Data.Array.IO     (IOArray)
import           Data.Array.MArray
import           Data.Monoid
import           Polysemy
import           Polysemy.Output
import           Polysemy.Random
import           Polysemy.Trace

import           Arvy.Algorithm
import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights

data Parameters = Parameters
  { nodeCount    :: Int
  , weights      :: WeightsParameter
  , initialTree  :: InitialTreeParameter
  , requestCount :: Int
  , requests     :: RequestsParameter
  , algorithm    :: Arvy
  }

runParams :: Parameters -> IO ()
runParams Parameters
  { nodeCount
  , weights = WeightsParameter { weightsGet }
  , initialTree = InitialTreeParameter { initialTreeGet }
  , requestCount
  , requests = requests
  , algorithm = algorithm
  } = runM $ runRandomIO $ do
  weights <- weightsGet nodeCount
  tree <- initialTreeGet nodeCount weights
  mutableTree <- sendM (thaw tree :: IO (IOArray Int (Maybe Int)))
  runTraceIO
    $ runOutputAsTrace @(Int, Int)
    $ runRequests @IO nodeCount weights mutableTree requests requestCount
    $ runArvyLocal @IO @IOArray weights mutableTree algorithm

traceMessages :: forall i r a . (Member Trace r, Member (Output (i, i)) r, Show i) => Sem r a -> Sem r a
traceMessages = intercept @(Output (i, i)) $ \case
  Output (from, to) -> do
    trace $ show from ++ " -> " ++ show to
    output (from, to)

-- | Measures distances
measureDistances :: (Ix i, IArray arr n, Num n) => arr i n -> Sem (Output i ': r) a -> Sem r (Sum n, a)
measureDistances weights = runFoldMapOutput (Sum . (weights !))
