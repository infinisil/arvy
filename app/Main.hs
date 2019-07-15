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
    , nodeCount = 100
    , requestCount = 10000
    , weights = Weights.unitEuclidian 2
    , requests = Requests.random
    , algorithm = Alg.ivy Tree.random
    }
  ]

main :: IO ()
main = forM_ params $ \par -> runM
  $ runTraceIO
  $ runParams par
  $ \n w -> mconcat
  --[ logging `runAs` trace
  [ sparseTreeStretchDiameter n w 1 `runAs` RunTrace (\(avg, diam) -> "Average tree stretch: " ++ show avg ++ ", tree diameter: " ++ show diam)
  --, (meanStddev . ratio w) `runAs` RunTrace (\(mean, stddev) -> "Request ratio mean: " ++ show mean ++ ", stddev: " ++ show stddev)
  --, (decayingFilter 1 . meanStddev . hopCount @Double) `runAs` RunTrace (\(mean, stddev) -> "Hop count mean: " ++ show mean ++ ", hop count stddev: " ++ show stddev)
  , (lastOne . ratio w) `runAs` RunTrace (\ratio -> "Ratio: " ++ show ratio)
  --, hopCount @Double `runAs` RunTrace show
  --, hopCount @Double `runAs` RunFile "hopcount" show
  --, (totalTreeWeight n w . everyNth 100 . Evaluation.Request.requests (const ())) `runAs` RunTrace (\ttw -> "Total tree weight: " ++ show ttw)
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
