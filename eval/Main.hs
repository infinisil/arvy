module Main where

import           Arvy
import           Arvy.Algorithm.Ivy
import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights

testParams :: Parameters
testParams = Parameters
  { nodeCount = 1000
  , weights = randomWeights
  , initialTree = mst
  , requestCount = 10000
  , requests = randomRequests
  , algorithm = ivy
  }

main :: IO ()
main = runParams testParams
