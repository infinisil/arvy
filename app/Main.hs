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
  $ inbetw

{-# INLINE utilFuns #-}
utilFuns :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
utilFuns = forM_ [ Parameters
    { randomSeed = 1
    , nodeCount = n
    , requestCount = 20000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.barabasiAlbert 1
    , Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    , Weights.clique
    , Weights.unitEuclidian 2
    , Weights.unitEuclidian 3
    , Weights.unitEuclidian 4
    , Weights.unitEuclidian 5
    ]
  , r <-
    [ Requests.random
    , Requests.pareto
    , Requests.farthest
    ]
  , n <- take 3 $ drop 8 $ iterate (*2) 1
  ] $ \par -> runEvals "utilFuns" par
    (baseAlgs ++
     [ Alg.utilityFun desc fun Tree.shortPairs
     | (desc, fun) <-
       [ ("w", \_ w -> w)
       , alpha 0.15 0.05
       , alpha 0.05 0.05
       , alpha 0.15 0.1
       , alpha 0.05 0.1
       ]
     ])
    [ ratio, treeWeight ]
  where
    baseAlgs = [ Alg.arrow Tree.mst
               , Alg.arrow Tree.shortPairs
               , Alg.ivy Tree.shortPairs
               , Alg.inbetweenWeighted 0.1 Tree.shortPairs
               , Alg.inbetweenWeighted 0.2 Tree.shortPairs
               , Alg.inbetweenWeighted 0.3 Tree.shortPairs
               ]
    {-# INLINE alpha #-}
    alpha :: Double -> Double -> (String, Int -> Double -> Double)
    alpha a m = ( "a" ++ show a ++ "," ++ show m
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
    , nodeCount = 500
    , requestCount = 10000
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

ivyBad :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
ivyBad = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = n
    , requestCount = 50000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.unitEuclidian 2
    , Weights.unitEuclidian 3
    , Weights.unitEuclidian 4
    , Weights.unitEuclidian 5
    , Weights.unitEuclidian 6
    , Weights.unitEuclidian 7
    , Weights.unitEuclidian 8
    , Weights.unitEuclidian 9
    , Weights.unitEuclidian 10
    ]
  , r <-
    [ Requests.random
    , Requests.pareto
    ]
  , n <- take 4 $ drop 8 $ iterate (*2) 1
  ] $ \par -> runEvals "ivyBad" par
    [ Alg.arrow Tree.random
    , Alg.arrow Tree.mst
    , Alg.arrow Tree.shortPairs
    , Alg.ivy Tree.mst
    ] [ ratio, requestHops, stretch, treeWeight ]

inbetw :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
inbetw = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = n
    , requestCount = 50000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.unitEuclidian 2
    , Weights.unitEuclidian 3
    , Weights.unitEuclidian 4
    , Weights.unitEuclidian 5
    , Weights.unitEuclidian 6
    , Weights.unitEuclidian 7
    , Weights.unitEuclidian 8
    , Weights.unitEuclidian 9
    , Weights.unitEuclidian 10
    ]
  , r <-
    [ Requests.random
    ]
  , n <- take 3 $ drop 8 $ iterate (*2) 1
  ] $ \par -> runEvals "inbetw" par
    [ Alg.inbetweenWeighted 0.1 Tree.mst
    , Alg.inbetweenWeighted 0.2 Tree.mst
    , Alg.inbetweenWeighted 0.3 Tree.mst
    , Alg.inbetweenWeighted 0.4 Tree.mst
    , Alg.ivy Tree.mst
    , Alg.arrow Tree.mst
    , Alg.arrow Tree.random
    , Alg.arrow Tree.shortPairs
    ] [ ratio, requestHops, stretch, treeWeight ]
