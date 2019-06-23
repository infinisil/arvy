module Main where

import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.ConstantRing
import           Arvy.Algorithm.Shortest
import           Arvy.Algorithm.Half
import           Arvy.Algorithm.Ivy

import           Parameters
import           ParametersLibrary
import           Polysemy
import           Polysemy.Random
import           Polysemy.Output
import           Polysemy.Trace
import Data.Monoid
import Arvy.Algorithm
import Evaluation

testParams :: Members '[Random, Lift IO] r => Parameters r
testParams = Parameters
  { nodeCount = 100
  , weights = pRingWeights
  , initialTree = pSemiCircles
  , requestCount = 10000
  , requests = pRandomRequests
  , algorithm = constantRing 4
  }

evaluation :: Evaluation ArvyEvent _
evaluation = distanceTraveled

main :: IO ()
main = do
  result <- runM $ runTraceIO $ runParams 0 testParams evaluation
  print result
