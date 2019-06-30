{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Arvy.Algorithm.Collection

import           Parameters
import           Polysemy
import           Polysemy.RandomFu
import           Polysemy.Output
import           Polysemy.Trace
import           Control.Category
import           Control.Monad
import qualified Debug.Trace as D
import           Evaluation
import           System.IO
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import           Evaluation.Tree

params :: Members '[RandomFu, Lift IO] r => [Parameters r]
params =
  [ Parameters
    { nodeCount = 500
    , requestCount = 10000
    , weights = Weights.barabasiAlbert 1
    , initialTree = Tree.mst
    , requests = Requests.random
    , algorithm = ivy
    }
  ]

main :: IO ()
main = forM_ params $ \par -> runM
  $ runTraceIO
  $ runOutputAsTrace
  $ runParams 0 par
  $ \n w t -> everyNth 100 >>> treeStretch n w t

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
