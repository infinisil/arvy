{-# LANGUAGE OverloadedStrings #-}

module Evaluation
  ( module Evaluation.Tree
  , module Evaluation.Utils
  , module Evaluation.Request
  , module Evaluation.Types
  , module Evaluation.Weights
  , module Evaluation
  ) where

import Evaluation.Request
import Evaluation.Tree
import Evaluation.Types
import Evaluation.Utils
import Evaluation.Weights
import Conduit
import qualified Data.Conduit.Combinators as C
import Polysemy


dataPoints :: Int
dataPoints = 100

allEvals :: Member (Lift IO) r => [ Eval r ]
allEvals = [ evalTreeDist, evalTreeRatio, evalTreeEdgeDist, evalRequestDist, evalRequestRatio, evalRequestHops ]

evalTreeDist :: Member (Lift IO) r => Eval r
evalTreeDist = Eval
  { evalName = "Average tree distance"
  , evalFun = \env@Env { envNodeCount } -> enumerate
      .| logFilter env dataPoints
      .| asConduit totalPairWeight env
      .| C.map (\((i, _), w) -> (fromIntegral i, w / fromIntegral (envNodeCount * (envNodeCount - 1) `div` 2)))
  }

evalTreeRatio :: Member (Lift IO) r => Eval r
evalTreeRatio = Eval
  { evalName = "Average tree ratio"
  , evalFun = \env -> enumerate
      .| logFilter env dataPoints
      .| asConduit avgTreeStretchDiameter env
      .| C.map (\((i, _), (str, _)) -> (fromIntegral i, str))
  }

evalTreeEdgeDist :: Member (Lift IO) r => Eval r
evalTreeEdgeDist = Eval
  { evalName = "Average tree edge distance"
  , evalFun = \env@Env { envNodeCount } -> enumerate
      .| logFilter env dataPoints
      .| asConduit totalTreeWeight env
      .| C.map (\((i, _), rat) -> (fromIntegral i, rat / fromIntegral (envNodeCount - 1)))
  }

evalRequestRatio :: Member (Lift IO) r => Eval r
evalRequestRatio = Eval
  { evalName = "Average request ratio"
  , evalFun = \env -> requestRatios env
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, rat) -> (fromIntegral i, rat))
  }

evalRequestDist :: Member (Lift IO) r => Eval r
evalRequestDist = Eval
  { evalName = "Average request distance"
  , evalFun = \env -> requestDists env
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, w) -> (fromIntegral i, w))
  }

evalRequestHops :: Member (Lift IO) r => Eval r
evalRequestHops = Eval
  { evalName = "Average request hop count"
  , evalFun = \env -> hopCount
      .| C.map fromIntegral
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, hops) -> (fromIntegral i, hops))
  }
