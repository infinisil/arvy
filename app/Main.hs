{-# LANGUAGE BangPatterns #-}

module Main where

import           Parameters
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import qualified Parameters.Algorithm as Alg

import Evaluation

import           Polysemy
import           Polysemy.RandomFu
import           Polysemy.Trace
import           Control.Monad
import           System.IO
import           Prelude
import Arvy.Local
import System.Directory
import Data.Ratio
import System.FilePath
import Polysemy.Async as PA
import Data.Functor
import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = runM $ runTraceIO $ runAsync
  --initialTreeMatters
  --inbetweenParameter
  --badIvy
  --genArrowTest
  testing


genArrowTest :: Members '[Async, Lift IO, Trace] r => Sem r ()
genArrowTest = do
  asyncs <- forM params $ \par -> do
    let stretchPath = "genArrowTest" </> paramFile par "stretch"
    stretchHandle <- liftIO $ createHandle stretchPath
    let ratioPath = "genArrowTest" </> paramFile par "ratio"
    ratioHandle <- liftIO $ createHandle ratioPath
    let weightPath = "genArrowTest" </> paramFile par "weight"
    weightHandle <- liftIO $ createHandle weightPath
    (env, cond) <- runParams par
    asyn <- async $ runConduit $ cond .| eval (stretchHandle, ratioHandle, weightHandle) env
    return $ asyn $> (stretchHandle, ratioHandle, weightHandle)
  forM_ asyncs (PA.await >=> (\(a, b, c) -> liftIO (hClose a) >> liftIO (hClose b) >> liftIO (hClose c)))
  where

  params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
  params =
    [ Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 1000000
      , weights = weights
      , requests = reqs
      , algorithm = alg
      }
    | weights <-
      [ --Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
       Weights.unitEuclidian 3
      ]
    , alg <-
      [ Alg.arrow Tree.mst
      , Alg.arrow Tree.shortPairs
      , Alg.arrow Tree.random
      , Alg.genArrow Tree.shortPairs
      , Alg.genArrow Tree.random
      ]
    , reqs <-
      [ Requests.random
      --, Requests.pareto
      ]
    ]
  eval :: Member (Lift IO) r => (Handle, Handle, Handle) -> Env -> ConduitT ArvyEvent Void (Sem r) [()]
  eval (stretchHandle, ratioHandle, weightHandle) Env { envNodeCount = n, envWeights = w, envTree = t } = ratio w
    .| sequenceConduits
    [ enumerate
      .| decayingFilter 4
      .| treeStretchDiameter n w t
      .| C.map (\((i, _), (stretch, _)) -> BS.pack $ show i ++ " " ++ show stretch)
      .| C.sinkHandle stretchHandle
    , movingAverage True 100
      .| enumerate
      .| decayingFilter 10
      .| C.map (\(i, rat) -> BS.pack $ show i ++ " " ++ show rat)
      .| C.sinkHandle ratioHandle
    , enumerate
      .| decayingFilter 8
      .| totalTreeWeight w t
      .| C.map (\((i, _), ttw) -> BS.pack $ show i ++ " " ++ show ttw)
      .| C.sinkHandle weightHandle
    , enumerate
      .| decayingFilter 10
      .| C.map (\(i, _) -> BS.pack $ show i)
      .| C.stdout
    ]

badIvy :: Members '[Async, Lift IO, Trace] r => Sem r ()
badIvy = do
  asyncs <- forM params $ \par -> do
    let ratioPath = "badIvy" </> paramFile par "ratio"
    ratioHandle <- liftIO $ createHandle ratioPath
    (env, cond) <- runParams par
    asyn <- async $ runConduit $ cond .| eval ratioHandle env
    return $ asyn $> ratioHandle
  forM_ asyncs (PA.await >=> liftIO . hClose)

  where

  params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
  params =
    [ Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100000
      , weights = weights
      , requests = reqs
      , algorithm = alg
      }
    | weights <-
      [ Weights.clique
      , Weights.ring
      , Weights.unitEuclidian 2
      , Weights.unitEuclidian 3
      , Weights.unitEuclidian 5
      , Weights.unitEuclidian 7
      , Weights.unitEuclidian 11
      , Weights.barabasiAlbert 1
      , Weights.barabasiAlbert 2
      , Weights.barabasiAlbert 4
      , Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
      , Weights.erdosRenyi (Weights.ErdosProbEpsilon 1)
      ]
    , alg <-
      [ Alg.arrow Tree.mst
      , Alg.inbetween (1 % 2) Tree.mst
      , Alg.inbetween (1 % 4) Tree.mst
      , Alg.inbetween (1 % 6) Tree.mst
      , Alg.ivy Tree.mst
      , Alg.random Tree.mst
      ]
    , reqs <-
      [ Requests.random
      , Requests.pareto
      , Requests.farthest
      ]
    ]
  eval :: Member (Lift IO) r => Handle -> Env -> ConduitT ArvyEvent Void (Sem r) [()]
  eval ratioHandle Env { envNodeCount = _n, envWeights = w, envTree = _t } = ratio w
    .| sequenceConduits
    [ movingAverage True 100
      .| enumerate
      .| decayingFilter 10
      .| C.map (\(i, rat) -> BS.pack $ show i ++ " " ++ show rat)
      .| C.sinkHandle ratioHandle
    , enumerate
      .| everyNth 10000
      .| C.map (\(i, _) -> BS.pack $ show i)
      .| C.stdout
    ] where


inbetweenParameter :: Members '[Async, Lift IO, Trace] r => Sem r ()
inbetweenParameter = do
  asyncs <- forM params $ \par -> do
    let ratioPath = "inbetweenParameter" </> paramFile par "ratio"
    ratioHandle <- liftIO $ createHandle ratioPath
    (env, cond) <- runParams par
    asyn <- async $ runConduit $ cond .| eval ratioHandle env
    return $ asyn $> ratioHandle
  forM_ asyncs (PA.await >=> liftIO . hClose)

  where

  params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
  params =
    [ Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100000
      , weights = weights
      , requests = reqs
      , algorithm = alg
      }
    | weights <-
      [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
      ]
    , alg <-
      [ Alg.arrow Tree.mst
      , Alg.inbetween (1 % 2) Tree.mst
      , Alg.inbetween (1 % 4) Tree.mst
      , Alg.inbetween (1 % 6) Tree.mst
      , Alg.inbetween (1 % 8) Tree.mst
      , Alg.ivy Tree.mst
      , Alg.random Tree.mst
      ]
    , reqs <-
      [ --Requests.random
      --Requests.pareto
      Requests.farthest
      ]
    ]
  eval :: Member (Lift IO) r => Handle -> Env -> ConduitT ArvyEvent Void (Sem r) [()]
  eval ratioHandle Env { envNodeCount = _n, envWeights = w, envTree = _t } = ratio w
    .| sequenceConduits
    [ movingAverage True 250
      .| enumerate
      .| decayingFilter 10
      .| C.map (\(i, rat) -> BS.pack $ show i ++ " " ++ show rat)
      .| C.sinkHandle ratioHandle
    , enumerate
      .| everyNth 2000
      .| C.map (\(i, _) -> BS.pack $ show i)
      .| C.stdout
    ] where

createHandle :: FilePath -> IO Handle
createHandle path = do
  createDirectoryIfMissing True (takeDirectory path)
  liftIO $ putStrLn $ "Opening handle to " ++ path
  openFile path WriteMode

testing :: Members '[Async, Lift IO, Trace] r => Sem r ()
testing = do
  (_, cond) <- runParams Parameters
    { randomSeed = 0
    , nodeCount = 20
    , requestCount = 10
    , weights = Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    , requests = Requests.random
    , algorithm = Alg.inbetween (1 % 2) Tree.random
    }
  runConduit $ cond .| C.print

initialTreeMatters :: Members '[Async, Lift IO, Trace] r => Sem r ()
initialTreeMatters = do
  asyncs <- forM params $ \par -> do
    let stretchPath = "initialTreeMatters" </> paramFile par "stretch"
    stretchHandle <- liftIO $ createHandle stretchPath
    let ratioPath = "initialTreeMatters" </> paramFile par "ratio"
    ratioHandle <- liftIO $ createHandle ratioPath
    (env, cond) <- runParams par
    asyn <- async $ runConduit $ cond .| eval (stretchHandle, ratioHandle) env
    return $ asyn $> (stretchHandle, ratioHandle)
  forM_ asyncs (PA.await >=> (\(a, b) -> liftIO (hClose a) >> liftIO (hClose b)))
  where

  params :: Members '[RandomFu, Lift IO, Trace] r => [Parameters r]
  params =
    [ Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100000
      , weights = weights
      , requests = reqs
      , algorithm = alg
      }
    | weights <-
      [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
      , Weights.ring
      ]
    , tree <-
      [ Tree.random
      , Tree.mst
      ]
    , alg <- ($ tree) <$>
      [ Alg.ivy
      , Alg.half
      ]
    , reqs <-
      [ Requests.random
      ]
    ]
  eval :: Member (Lift IO) r => (Handle, Handle) -> Env -> ConduitT ArvyEvent Void (Sem r) [()]
  eval (stretchHandle, ratioHandle) Env { envNodeCount = n, envWeights = w, envTree = t } = ratio w
    .| sequenceConduits
    [ enumerate
      .| decayingFilter 4
      .| treeStretchDiameter n w t
      .| C.map (\((i, _), (stretch, _)) -> BS.pack $ show i ++ " " ++ show stretch)
      .| C.sinkHandle stretchHandle
    , movingAverage True 100
      .| enumerate
      .| decayingFilter 10
      .| C.map (\(i, rat) -> BS.pack $ show i ++ " " ++ show rat)
      .| C.sinkHandle ratioHandle
    , enumerate
      .| decayingFilter 10
      .| C.map (\(i, _) -> BS.pack $ show i)
      .| C.stdout
    ] where

toFile :: MonadIO m => FilePath -> ConduitT BS.ByteString Void m ()
toFile path = do
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ putStrLn $ "Writing to " ++path
  handle <- liftIO $ openFile path WriteMode
  () <- C.sinkHandle handle
  liftIO $ hClose handle
