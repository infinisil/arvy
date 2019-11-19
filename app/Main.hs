{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parameters
import qualified Parameters.Algorithm      as Alg
import qualified Parameters.Requests       as Requests
import qualified Parameters.Tree           as Tree
import qualified Parameters.Weights        as Weights

import           Colog
import           Evaluation

import           Arvy.Algorithm.Collection
import           Arvy.Log
import           Control.Monad
import           Evaluation.Plot
import           Polysemy
import           Polysemy.Async
import           Prelude

main :: IO ()
main = do
  --trees
  --ratio2
  --ratio3
  --algs
  adversary
  --ivyClique
  --recliqueProg
  return ()

trees :: IO ()
trees = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           ]
      }
    , genParamNodeCount = 10
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.arrow, Tree.shortPairs)
      , (Alg.arrow, Tree.shortestPairs)
      , (Alg.arrow, Tree.random)
      , (Alg.arrow, Tree.mst)
      ]
    }
  writeResultsToDats "trees" results

converging :: IO ()
converging = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           , evalTreeEdgeDist
                           ]
      }
    , genParamNodeCount = 100
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.arrow, Tree.mst)
      , (Alg.arrow, Tree.random)

      , (Alg.ivy, Tree.random)
      , (Alg.random, Tree.random)
      , (Alg.localMinPairs, Tree.random)
      , (Alg.minWeight, Tree.random)
      , (Alg.dynamicStar, Tree.random)
      ]
    }
  writeResultsToDats "converging" results

ratio :: IO ()
ratio = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 1000000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           , evalTreeEdgeDist
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.inbetween (0 % 8), Tree.random)
      , (Alg.inbetween (1 % 8), Tree.random)
      , (Alg.inbetween (2 % 8), Tree.random)
      , (Alg.inbetween (3 % 8), Tree.random)
      , (Alg.inbetween (4 % 8), Tree.random)
      , (Alg.inbetween (5 % 8), Tree.random)
      , (Alg.inbetween (6 % 8), Tree.random)
      , (Alg.inbetween (7 % 8), Tree.random)
      , (Alg.inbetween (8 % 8), Tree.random)
      ]
    }
  writeResultsToDats "ratio" results

ratio2 :: IO ()
ratio2 = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           , evalTreeEdgeDist
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.inbetweenWeighted 0, Tree.random)
      , (Alg.inbetweenWeighted 0.125, Tree.random)
      , (Alg.inbetweenWeighted 0.25, Tree.random)
      , (Alg.inbetweenWeighted 0.375, Tree.random)
      , (Alg.inbetweenWeighted 0.5, Tree.random)
      , (Alg.inbetweenWeighted 0.625, Tree.random)
      , (Alg.inbetweenWeighted 0.75, Tree.random)
      , (Alg.inbetweenWeighted 0.875, Tree.random)
      , (Alg.inbetweenWeighted 1.0, Tree.random)
      ]
    }
  writeResultsToDats "ratio2" results

algs :: IO ()
algs = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.ivy, Tree.random)
      , (Alg.localMinPairs, Tree.random)
      , (Alg.inbetween (3 % 4), Tree.random)
      ]
    }
  writeResultsToDats "algs" results

adversary :: IO ()
adversary = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 1000000
      , sharedParamRequests = Requests.farthestRatio
      , sharedParamEvals = [ evalTime
                           , evalRatio
                           , evalHops
                           ]
      }
    , genParamNodeCount = 100
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.ivy, Tree.random)
      , (Alg.localMinPairs, Tree.random)
      , (Alg.inbetween (3 % 4), Tree.random)
      , (Alg.dynamicStar, Tree.random)
      ]
    }
  writeResultsToDats "adversary" results

ivyClique :: IO ()
ivyClique = forM_ [3..8] runIt where
  runIt :: Int -> IO ()
  runIt i = do
    results <- runM .@ runAsyncInIO $ runLogBySeverity Info (cmap messageText logTextStdout) $ runGenParams GenParams
      { genParamShared = SharedParams
        { sharedParamRandomSeed = 0
        , sharedParamRequestCount = 1000000
        , sharedParamRequests = Requests.random
        , sharedParamEvals = [ evalTime
                            , evalHops
                            , evalTreeEdgeDist
                            ]
        }
      , genParamNodeCount = i
      , genParamWeights = Weights.clique
      , genParamAlgs =
        [ (Alg.arrow, Tree.shortestPairs)
        , (Alg.ivy, Tree.shortestPairs)
        ]
      }
    writeResultsToDats ("low" <> tshow i) results

recliqueProg :: IO ()
recliqueProg = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runSpecParams SpecParams
    { specParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 1000000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalHops
                           , evalTreeEdgeDist
                           ]
      }
    , specParamAlg = Alg.reclique
    , specParamInit = RecliqueConf
      { recliqueFactor = 4
      , recliqueLevels = 7
      , recliqueBase = 3
      }
    , specParamGenAlgs =
      [ (Alg.ivy, Tree.random)
      , (Alg.arrow, Tree.bestStar)
      , (Alg.localMinPairs, Tree.random)
      , (Alg.inbetween (3 % 4), Tree.random)
      ]
    }
  writeResultsToDats "reclique" results
