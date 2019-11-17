{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where

import           Parameters
import qualified Parameters.Algorithm      as Alg
import qualified Parameters.Requests       as Requests
import qualified Parameters.Tree           as Tree
import qualified Parameters.Weights        as Weights
import           System.Environment
import           System.IO

import           Colog
import qualified Data.Text                 as T
import           Evaluation

import           Arvy.Algorithm
import           Arvy.Algorithm.Collection
import           Arvy.Log
import           Control.Monad
import           Data.Array.Unboxed
import           Data.Ix
import           Evaluation.Plot
import           Polysemy
import           Polysemy.Async
import           Polysemy.RandomFu
import           Prelude
import           Utils

data RingNodeData = RingNodeData
  { ringSuccessor :: Node
  , ringWeights   :: Node -> Weight
  }

ringTree :: NodeCount -> ArvyData ()
ringTree n = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = \node -> ArvyNodeData
    { arvyNodeSuccessor = if node == 0 then 0 else node - 1
    , arvyNodeAdditional = ()
    , arvyNodeWeights = \other ->
        let
          low = min node other
          mid = max node other
          high = low + n
          dist = min (mid - low) (high - mid)
        in fromIntegral dist
    }
  }

drawTree :: LogMember r => T.Text -> RootedTree -> Sem r ()
drawTree name tree = do
  lgInfo $ "\\nextgroupplot[title=" <> name <> "]"
  lgInfo "\\treenodes"
  forM_ (assocs tree) $ \(a, b) ->
    if a /= b then
      lgInfo $ "\\draw[thick] (" <> tshow a <> ") -- (" <> tshow b <> ");"
    else
      lgDebug $ "Root is " <> tshow a

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [read -> seed] <- getArgs
  runM $ runRandomSeed seed $ runLogBySeverity Info (cmap messageText logTextStdout) $ do
    let n = 10
    lgInfo "\\newcommand{\\treenodes}{"
    weights <- Weights.weightsGen (Weights.unitEuclidian 2) n
    lgInfo "}"
    Tree.treeGen Tree.mst n weights >>= drawTree "MST"
    Tree.treeGen Tree.bestStar n weights >>= drawTree "Best Star"
    Tree.treeGen Tree.shortestPairs n weights >>= drawTree "Pair Minimizer"
    Tree.treeGen Tree.shortPairs n weights >>= drawTree "Approx Pair Minimizer"

    return ()

main'' :: IO ()
main'' = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runSpecParams SpecParams
    { specParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 10000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [ evalRequestDist'
                           , evalRequestRatio
                           , evalRequestHops
                           , evalTreeEdgeDist
                           ]
      }
    --, specParamNodeCount = 1000
    --, specParamWeights = Weights.unitEuclidian 2
    , specParamAlg = Alg.reclique
    , specParamInit = RecliqueConf
      { recliqueFactor = 3
      , recliqueLevels = 5
      , recliqueBase = 3
      }
    , specParamGenAlgs =
      [
      (Alg.dynamicStar, Tree.shortPairs),
      (Alg.arrow, Tree.shortPairs),
      (Alg.arrow, Tree.bestStar),
      (Alg.localMinPairs, Tree.shortPairs),
      (Alg.indexMeanHop, Tree.shortPairs),
      (Alg.indexMeanWeight, Tree.shortPairs)
      ]
    }
  plotResults "wip" results

main' :: IO ()
main' = do
  results <- runM .@ runAsyncInIO
    $ runLogBySeverity Info (cmap messageText logTextStdout)
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 100000
      , sharedParamRequests = Requests.farthestRatio
      , sharedParamEvals = [ evalRequestDist
                           , evalRequestRatio
                           , evalRequestHops
                           ]
      }
    , genParamNodeCount = 100
    , genParamWeights = Weights.unitEuclidian 2
    , genParamAlgs =
      [
      (Alg.dynamicStar, Tree.random),
      (Alg.arrow, Tree.shortPairs),
      (Alg.arrow, Tree.bestStar),
      (Alg.ivy, Tree.random),
      (Alg.localMinPairs, Tree.random)
      ]
    }
  plotResults "wip" results

--main :: IO ()
--main = runM
--  . runTraceIO
--  .@ PA.runAsyncInIO
--  $ best

--best :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
--best = forM_ [ Parameters
--    { randomSeed = 0
--    , nodeCount = 1000
--    , requestCount = 100000
--    , weights = w
--    , requests = r
--    }
--  | w <-
--    --[ Weights.unitEuclidian 2
--    --[Weights.erdosRenyi (Weights.ErdosProbEpsilon 0) ]
--    [ Weights.barabasiAlbert 1 ]
--    --] Weights.ring
--    --]
--  , r <-
--    [ Requests.random
--    ]
--  ] $ \par -> runEvals "best" par
--    [ Alg.arrow Tree.random
--    , Alg.arrow Tree.mst
--    , Alg.arrow Tree.shortPairs
--    , Alg.arrow Tree.bestStar
--    , Alg.ivy Tree.random
--    , Alg.indexMeanScore WeightSumBased (const 0.2) (Tree.random' initialIndexMeanState)
--    , Alg.localMinPairs Tree.random
--    , Alg.inbetweenWeighted 0.8 Tree.random
--    , Alg.inbetweenWeighted 0.2 Tree.random
--    , Alg.genArrow Tree.random
--    ] allEvals
--
--
--
--testing :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
--testing = runEvals "testing" params algs allEvals
--  where
--    params = Parameters
--      { randomSeed = 0
--      , nodeCount = 1000
--      , requestCount = 100000
--      , weights = Weights.unitEuclidian 2
--      , requests = Requests.random
--      }
--    algs = [ Alg.arrow Tree.bestStar
--           , Alg.arrow Tree.mst
--           , Alg.arrow Tree.shortPairs
--           , Alg.arrow Tree.random
--           ]
