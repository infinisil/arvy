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
import Control.Category
import Data.Monoid
import qualified Debug.Trace as D
import Control.Applicative
import Arvy.Algorithm
import Evaluation

testParams :: Members '[Random, Lift IO] r => Parameters r
testParams = Parameters
  { nodeCount = 100
  , weights = pRandom2DWeights
  , initialTree = pRing
  , requestCount = 10000
  , requests = pRandomRequests
  , algorithm = ivy
  }

main :: IO ()
main = runM
  $ runTraceIO
  $ runOutputAsTrace
  $ runParams 0 testParams
  $ \n w -> everyNth 100 >>> treeStretch n w

trace' :: Show b => Eval a b -> Eval a b
trace' = fmap $ \v -> D.trace (show v) v
