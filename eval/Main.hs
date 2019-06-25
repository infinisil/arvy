{-# LANGUAGE BlockArguments #-}

module Main where

import           Arvy.Algorithm.Collection

import           Parameters
import           ParametersLibrary
import           Polysemy
import           Polysemy.Random
import           Polysemy.Output
import           Polysemy.Trace
import           Control.Category
import           Control.Monad
import qualified Debug.Trace as D
import           Evaluation
import           System.IO

params :: Members '[Random, Lift IO] r => [Parameters r]
params =
  [ Parameters
    { nodeCount = 500
    , weights = pBarabasiWeights 1
    , initialTree = pMst
    , requestCount = 10000
    , requests = pRandomRequests
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
