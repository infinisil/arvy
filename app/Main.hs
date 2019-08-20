module Main where

import           Parameters
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import qualified Parameters.Algorithm as Alg

import Evaluation

import Polysemy
import Polysemy.Trace
import Control.Monad
import System.IO
import Prelude
import qualified Polysemy.Async as PA

main :: IO ()
main = runM
  . runTraceIO
  .@ PA.runAsyncInIO
  $ utilFuns

{-# INLINE utilFuns #-}
utilFuns :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
utilFuns = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 100
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.unitEuclidian 2
    ]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "utilFuns" par
    (baseAlgs ++
     [ Alg.utilityFun desc fun Tree.random
     | (desc, fun) <-
       [ ("weight", \_ w -> w)
       , alpha 0.5 0.1
       , alpha 0.25 0.1
       , alpha 0.125 0.1
       , alpha 0.5 0.05
       , alpha 0.25 0.05
       , alpha 0.125 0.05
       ]
     ])
    [ ratio, treeWeight ]
  where
    baseAlgs = [ Alg.arrow Tree.mst
               , Alg.arrow Tree.shortPairs
               ]
    {-# INLINE alpha #-}
    alpha :: Double -> Double -> (String, Int -> Double -> Double)
    alpha a m = ( "alpha" ++ show a ++ "," ++ show m
                , \i w -> w * (1 + m * (1 - exp (a * fromIntegral i))))

genArrow :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
genArrow = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 500
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    , Weights.unitEuclidian 2
    , Weights.unitEuclidian 3
    ]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "genArrow" par
    [ Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    ]
    [ stretch, ratio, treeWeight ]

converges :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
converges = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 100
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    ]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "converges" par
    [ Alg.ivy Tree.mst
    , Alg.ivy Tree.shortPairs
    , Alg.ivy Tree.random
    , Alg.random Tree.mst
    , Alg.random Tree.shortPairs
    , Alg.random Tree.random
    ]
    [ ratio ]


testing :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
testing = runEvals "testing " params algs evals
  where
    params = Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100
      , weights = Weights.unitEuclidian 2
      , requests = Requests.random
      }
    algs = [ Alg.arrow Tree.mst
           , Alg.ivy Tree.mst
           , Alg.inbetweenWeighted 0.5 Tree.mst]
    evals = [stretch, ratio, treeWeight]
