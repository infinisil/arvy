module Main where

import           Parameters
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import qualified Parameters.Algorithm as Alg
import Arvy.Algorithm.Collection

import Evaluation

import Polysemy
import Polysemy.Trace
import Control.Monad
import System.IO
import Prelude
import qualified Polysemy.Async as PA
import Polysemy.RandomFu

main :: IO ()
main = runM
  . runTraceIO
  .@ PA.runAsyncInIO
  $ best

best :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
best = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 1000
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    --[ Weights.unitEuclidian 2
    --[Weights.erdosRenyi (Weights.ErdosProbEpsilon 0) ]
    [ Weights.barabasiAlbert 1 ]
    --] Weights.ring
    --]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "best" par
    [ Alg.arrow Tree.random
    , Alg.arrow Tree.mst
    , Alg.arrow Tree.shortPairs
    , Alg.arrow Tree.bestStar
    , Alg.ivy Tree.random
    , Alg.indexMeanScore WeightSumBased (const 0.2) (Tree.random' initialIndexMeanState)
    , Alg.localMinPairs Tree.random
    , Alg.inbetweenWeighted 0.8 Tree.random
    , Alg.inbetweenWeighted 0.2 Tree.random
    , Alg.genArrow Tree.random
    ] allEvals



testing :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
testing = runEvals "testing" params algs allEvals
  where
    params = Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100000
      , weights = Weights.unitEuclidian 2
      , requests = Requests.random
      }
    algs = [ Alg.arrow Tree.bestStar
           , Alg.arrow Tree.mst
           , Alg.arrow Tree.shortPairs
           , Alg.arrow Tree.random
           ]
