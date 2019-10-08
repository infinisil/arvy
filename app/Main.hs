{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Arvy.Algorithm.Collection
import           Parameters
import qualified Parameters.Algorithm      as Alg
import qualified Parameters.Requests       as Requests
import qualified Parameters.Tree           as Tree
import qualified Parameters.Weights        as Weights

import           Evaluation

import           Arvy.Algorithm
import           Arvy.Log
import           Arvy.Weight
import           Control.Monad
import           Polysemy
import qualified Polysemy.Async            as PA
import           Polysemy.RandomFu
import           Polysemy.Trace
import           Prelude
import           System.IO
import           Utils
import Data.List (intercalate)
import Evaluation.Plot

data RingNodeData = RingNodeData
  { ringSuccessor :: Node
  , ringWeights   :: Node -> Weight
  }

instance Show (ArvyData RingNodeData) where
  show (ArvyData { .. }) = concatMap showNode [0..arvyDataNodeCount - 1] where
    showNode :: Node -> String
    showNode node = "  " ++ show node ++ " -> " ++ show ringSuccessor
      ++ ", weights: " ++ intercalate ", " (map (show . ringWeights) [0..arvyDataNodeCount - 1])
      ++ "\n"
      where RingNodeData { .. } = arvyDataNodeData node

instance HasSuccessor RingNodeData where
  getSuccessor = ringSuccessor

instance HasWeights RingNodeData where
  getWeights = ringWeights

instance HasState RingNodeData ()

ringTree :: NodeCount -> ArvyData RingNodeData
ringTree n = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = \node -> RingNodeData
    { ringSuccessor = if node == 0 then 0 else node - 1
    , ringWeights = \other ->
        let
          low = min node other
          mid = max node other
          high = low + n
          dist = min (mid - low) (high - mid)
        in fromIntegral dist
    }
  }

main :: IO ()
main = do
  results <- runM
    $ runRandomSeed 0
    $ runTraceIO
    $ runIgnoringLog
    $ runGenParams GenParams
    { genParamShared = SharedParams
      { sharedParamRandomSeed = 0
      , sharedParamRequestCount = 10000
      , sharedParamRequests = Requests.random
      , sharedParamEvals = [evalRequestHops]
      }
    , genParamInit = ringTree 10000
    , genParamAlgs = [Alg.arrow]
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
