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
  trees
  converging
  ratio
  ratio2
  algs
  adversary
  ivyClique
  recliqueProg

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
                           , evalHops
                           ]
      }
    , genParamNodeCount = 10
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.arrow, Tree.approxMinPairs)
      , (Alg.arrow, Tree.minPairs)
      , (Alg.arrow, Tree.random)
      , (Alg.arrow, Tree.mst)
      ]
    }
  writeResultsToDats "trees" results

-- Put 0.52140543316472 into the arrow-random column
-- https://math.stackexchange.com/questions/1254129/average-distance-between-two-random-points-in-a-square
converging :: IO ()
converging = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTreeEdgeDist
                           ]
      }
    , genParamNodeCount = 100
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.random)
      , (Alg.arrow, Tree.bestStar)
      , (Alg.arrow, Tree.mst)

      , (Alg.random, Tree.random)
      , (Alg.ivy, Tree.random)
      , (Alg.dynamicStar, Tree.random)
      , (Alg.localMinPairs, Tree.random)
      , (Alg.edgeMin, Tree.random)
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
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.fixedRatio (0 % 8), Tree.random)
      , (Alg.fixedRatio (1 % 8), Tree.random)
      , (Alg.fixedRatio (2 % 8), Tree.random)
      , (Alg.fixedRatio (3 % 8), Tree.random)
      , (Alg.fixedRatio (4 % 8), Tree.random)
      , (Alg.fixedRatio (5 % 8), Tree.random)
      , (Alg.fixedRatio (6 % 8), Tree.random)
      , (Alg.fixedRatio (7 % 8), Tree.random)
      , (Alg.fixedRatio (8 % 8), Tree.random)
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
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.weightedFixedRatio 0, Tree.random)
      , (Alg.weightedFixedRatio 0.125, Tree.random)
      , (Alg.weightedFixedRatio 0.25, Tree.random)
      , (Alg.weightedFixedRatio 0.375, Tree.random)
      , (Alg.weightedFixedRatio 0.5, Tree.random)
      , (Alg.weightedFixedRatio 0.625, Tree.random)
      , (Alg.weightedFixedRatio 0.75, Tree.random)
      , (Alg.weightedFixedRatio 0.875, Tree.random)
      , (Alg.weightedFixedRatio 1.0, Tree.random)
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
      , sharedParamRequestCount = 1000000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalTime
                           , evalHops
                           ]
      }
    , genParamNodeCount = 1000
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.ivy, Tree.random)
      , (Alg.localMinPairs, Tree.random)
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
                           , evalHops
                           ]
      }
    , genParamNodeCount = 100
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [ (Alg.arrow, Tree.bestStar)
      , (Alg.ivy, Tree.random)
      , (Alg.localMinPairs, Tree.random)
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
                             ]
        }
      , genParamNodeCount = i
      , genParamWeights = Weights.clique
      , genParamAlgs =
        [ (Alg.arrow, Tree.minPairs)
        , (Alg.ivy, Tree.minPairs)
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
      , (Alg.arrow, Tree.mst)
      , (Alg.localMinPairs, Tree.random)
      ]
    }
  writeResultsToDats "reclique" results
