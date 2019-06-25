{-# LANGUAGE BlockArguments #-}

module Main where

import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.ConstantRing
import           Arvy.Algorithm.Shortest
import           Arvy.Algorithm.Half
import           Arvy.Algorithm.Ivy

import           Parameters
import           ParametersLibrary
import           Polysemy
import           Polysemy.Random
import           Polysemy.Output
import           Polysemy.Trace
import Control.Category
import Data.Monoid
import qualified Debug.Trace as D
import Control.Applicative
import Arvy.Algorithm
import Evaluation
import System.IO

testParams :: Members '[Random, Lift IO] r => Parameters r
testParams = Parameters
  { nodeCount = 100
  , weights = pRandom2DWeights
  , initialTree = pRing
  , requestCount = 100000
  , requests = pRandomRequests
  , algorithm = half
  }

main :: IO ()
main = runM
  $ runTraceIO
  $ runOutputToFile "eval-output"
  $ mapOutput show
  $ runParams 0 testParams
  $ \n w -> everyNth 10 >>> treeStretch n w

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
