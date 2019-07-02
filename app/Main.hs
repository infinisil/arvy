{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Arvy.Algorithm.Collection

import           Parameters
import           Polysemy
import           Polysemy.RandomFu
import           Polysemy.Output
import           Polysemy.Trace
import Data.Monoid
import Arvy.Local
import Data.Array.IO
import           Control.Category
import           Control.Monad
import qualified Debug.Trace as D
import           Evaluation
import           System.IO
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import           Evaluation.Tree

params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
params =
  [ Parameters
    { nodeCount = 1000
    , requestCount = 100000
    , weights = Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    , initialTree = Tree.mst
    , requests = Requests.random
    , algorithm = ivy
    }
  ]

main :: IO ()
main = forM_ params $ \par -> runM
  $ runTraceIO
  $ traceOutput resultShower
  $ runParams 0 par
  $ evaluation
  where
    evaluation :: NodeCount -> GraphWeights -> IOArray Int (Maybe Int) -> Eval ArvyEvent ((Int, Request (Sum Int)), Double)
    evaluation n w t = requestHops -- Get requests while counting their hops
      >>> enumerate -- Enumerate all requests
      >>> everyNth 100 -- Only process every 100th one
      >>> treeStretch n w t -- And calculate the tree stretch too
    resultShower :: ((Int, Request (Sum Int)), Double) -> String
    resultShower ((n, Request { path = Sum hops }), stretch) = "[" ++ show n ++ "] Hop count: " ++ show hops ++ ", tree stretch: " ++ show stretch



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

trace' :: Show b => Eval a b -> Eval a b
trace' = fmap $ \v -> D.trace (show v) v
