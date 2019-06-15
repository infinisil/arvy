module Main where

import           Arvy
import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.ConstantRing
import           Arvy.Algorithm.Ivy
import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights

testParams :: Parameters
testParams = Parameters
  { nodeCount = 1000
  , weights = ringWeights
  , initialTree = mst
  , requestCount = 100000
  , requests = randomRequests
  , algorithm = ivy
  }

main :: IO ()
main = runParams 0 testParams
