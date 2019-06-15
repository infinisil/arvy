module Main where

import           Arvy.Algorithm.Ivy

import           Parameters
import           ParametersLibrary

testParams :: Parameters
testParams = Parameters
  { nodeCount = 10
  , weights = pRingWeights
  , initialTree = pMst
  , requestCount = 10
  , requests = pRandomRequests
  , algorithm = ivy
  }


main :: IO ()
main = runParams 0 testParams
