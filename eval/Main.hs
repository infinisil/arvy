module Main where

import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.ConstantRing
import           Arvy.Algorithm.Half
import           Arvy.Algorithm.Ivy

import           Parameters
import           ParametersLibrary
import           Polysemy
import           Polysemy.Random

testParams :: Members '[Random, Lift IO] r => Parameters r
testParams = Parameters
  { nodeCount = 10
  , weights = pRingWeights
  , initialTree = pSemiCircles
  , requestCount = 100
  , requests = pRandomRequests
  , algorithm = constantRing 4
  }


main :: IO ()
main = runM $ runParams 0 testParams
