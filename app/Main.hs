{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Parameters
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import qualified Parameters.Algorithm as Alg

import           Evaluation
import           Evaluation.Tree
import Evaluation.Utils
import Evaluation.Request

import           Polysemy
import           Polysemy.RandomFu
import           Polysemy.Output
import           Polysemy.Trace
import           Data.Monoid
import           Control.Category
import           Control.Monad
import           System.IO
import           Prelude hiding ((.), id)

params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
params =
  [ Parameters
    { randomSeed = 0
    , nodeCount = 1000
    , requestCount = 10000
    , weights = Weights.unitEuclidian 3
    , requests = Requests.pareto
    , algorithm = Alg.ivy Tree.mst
    }
  ]

main :: IO ()
main = forM_ params $ \par -> runM
  $ runTraceIO
  $ runParams par
  $ \n w -> mconcat
  --[ logging `runAs` trace
  [ sparseTreeStretchDiameter n w 1 `runAs` (\(avg, diam) -> trace $ "Average tree stretch: " ++ show avg ++ ", tree diameter: " ++ show diam)
  , (meanStddev . ratio w) `runAs` (\(mean, stddev) -> trace $ "Request ratio mean: " ++ show mean ++ ", stddev: " ++ show stddev)
  , (meanStddev . hopCount @Double) `runAs` (\(mean, stddev) -> trace $ "Hop count mean: " ++ show mean ++ ", stddev: " ++ show stddev)
  , (totalTreeWeight n w . everyNth 100 . Evaluation.Request.requests (const ())) `runAs` (\ttw -> trace $ "Total tree weight: " ++ show ttw)
  ]

traceOutput :: Member Trace r => (x -> String) -> Sem (Output x ': r) a -> Sem r a
traceOutput f = interpret \case Output x -> trace $ f x

mapOutput :: Member (Output y) r => (x -> y) -> Sem (Output x ': r) a -> Sem r a
mapOutput f = interpret \case Output x -> output $ f x

runOutputToFile :: Member (Lift IO) r => FilePath -> Sem (Output String ': r) a -> Sem r a
runOutputToFile file sem = do
  handle <- sendM $ openFile file WriteMode
  res <- interpret
    \case Output str -> sendM $ hPutStrLn handle str
    sem
  sendM $ hClose handle
  return res
