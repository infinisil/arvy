{-# LANGUAGE BlockArguments #-}

module Main where

import           Arvy.Algorithm.Collection

import Prelude hiding ((.))
import           Parameters
import           ParametersLibrary
import           Polysemy
import           Polysemy.Random
import           Polysemy.Output
import           Polysemy.Trace
import           Polysemy.State
import           Control.Category
import           Control.Monad
import qualified Debug.Trace as D
import           Evaluation
import           System.IO

params :: Members '[Random, Lift IO] r => [Parameters r]
params =
  [ Parameters
    { nodeCount = 200
    , weights = pErdosRenyi
    , initialTree = pMst
    , requestCount = 10000
    , requests = pRandomRequests
    , algorithm = ivy
    }
  ]

main :: IO ()
main = forM_ params $ \par -> runM
  $ runTraceIO
  $ runOutputToFile "values"
  $ mapOutput (\(v, a) -> show v ++ " " ++ show a)
  $ movingAverage 10
  $ runParams 0 par
  $ \n w t -> everyNth 1 >>> treeStretch n w t

movingAverage :: Member (Output (Double, Double)) r => Int -> Sem (Output Double ': r) a -> Sem r a
movingAverage c = fmap snd . runState @[Double] [] . reinterpret \case
  Output o -> do
    all <- gets (take c . (o:))
    let avg = sum all / fromIntegral (length all)
    put all
    output (o, avg)

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
