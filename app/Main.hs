{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

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
import           Polysemy.State
import           Polysemy.Output
import           Polysemy.Trace
import           Data.Monoid
import           Control.Category
import           Control.Monad
import           System.IO
import           Prelude hiding ((.), id)
import Pipes
import qualified Pipes.Prelude as P
import Data.Functor

params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
params =
  [ Parameters
    { randomSeed = 0
    , nodeCount = 1000
    , requestCount = 1000
    , weights = Weights.ring
    , requests = Requests.random
    , algorithm = alg
    }
  | alg <- [ --Alg.ivy Tree.random
           -- Alg.ivy Tree.mst
           Alg.constantRing
           ]
  ]

main :: IO ()
main = runM
  $ runTraceIO
  $ forM_ params
  $ \par -> runParams par
  $ \n w -> distribute
  [ Evaluation.Utils.enumerate >-> P.show >-> P.stdoutLn

  --Evaluation.Request.requests (const ()) >-> decayingFilter 1 >-> treeStretchDiameter n w >-> P.map (\(s, d) -> show s ++ " " ++ show d) >-> P.stdoutLn
  , Evaluation.Request.requests (const ()) >-> Evaluation.Utils.enumerate >-> everyNth 100 >-> P.map (show . fst) >-> P.stdoutLn

  --[ --sparseTreeStretchDiameter n w 1 `runAs` RunTrace (\(avg, diam) -> "Average tree stretch: " ++ show avg ++ ", tree diameter: " ++ show diam)
    --(enumerate . (meanStddevList <$> (batch 100 . ratio w))) `runAs` RunFile ("ratio" ++ Alg.algorithmName (algorithm par)) (\(n, (mean, stddev)) -> show n ++ " " ++ show mean ++ " " ++ show stddev)
  --, (decayingFilter 1 . meanStddev . hopCount @Double) `runAs` RunTrace (\(mean, stddev) -> "Hop count mean: " ++ show mean ++ ", hop count stddev: " ++ show stddev)
  --, (lastOne . ratio w) `runAs` RunTrace (\ratio -> "Ratio: " ++ show ratio)
  --, (everyNth 10 . enumerate . logging . Evaluation.Request.requests (const ())) `runAs` RunTrace (\(n, _) -> show n)
   --(treeStretchDiameter n w . Evaluation.Request.requests (const ())) `runAs` RunTrace (\(s, d) -> show s ++ " " ++ show d)
  --    (\((k, _), (str, _)) -> show k ++ " " ++ show str)
  ]

-- Forwards all values to all given consumers
distribute :: (Monad m, Foldable f) => f (Consumer i m ()) -> Consumer i m ()
distribute = foldr (\con rest -> P.tee con >-> rest) P.drain

  --, hopCount @Double `runAs` RunTrace show
  --, hopCount @Double `runAs` RunFile "hopcount" show
  --, (totalTreeWeight n w . everyNth 100 . Evaluation.Request.requests (const ())) `runAs` RunTrace (\ttw -> "Total tree weight: " ++ show ttw)

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
