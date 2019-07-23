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
import           Prelude hiding ((.), id)
import Pipes hiding (enumerate)
import qualified Pipes.Prelude as P
import Arvy.Local
import Data.Array.IO
import System.Directory
import System.FilePath

main :: IO ()
main = runM $ runTraceIO
  initialTreeMatters

createHandle :: FilePath -> IO Handle
createHandle path = do
  createDirectoryIfMissing True (takeDirectory path)
  liftIO $ putStrLn $ "Opening handle to " ++ path
  openFile path WriteMode

initialTreeMatters :: Members '[Lift IO, Trace] r => Sem r ()
initialTreeMatters = forM_ params $ \par -> do
  let stretchPath = "initialTreeMatters" </> paramFile par "stretch"
  stretchHandle <- liftIO $ createHandle stretchPath
  let ratioPath = "initialTreeMatters" </> paramFile par "ratio"
  ratioHandle <- liftIO $ createHandle ratioPath
  runParams par
    $ eval (stretchHandle, ratioHandle)
  liftIO $ hClose stretchHandle
  liftIO $ hClose ratioHandle

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
  eval :: Member (Lift IO) r => (Handle, Handle) -> Int -> GraphWeights -> IOUArray Node Node -> Consumer ArvyEvent (Sem r) ()
  eval (stretchHandle, ratioHandle) n w t = ratio w
    >-> distribute
    [ enumerate
      >-> decayingFilter 4
      >-> treeStretchDiameter n w t
      >-> P.map (\((i, _), (stretch, _)) -> show i ++ " " ++ show stretch)
      >-> P.toHandle stretchHandle
    , movingAverage True 100
      >-> enumerate
      >-> decayingFilter 10
      >-> P.map (\(i, rat) -> show i ++ " " ++ show rat)
      >-> P.toHandle ratioHandle
    , enumerate
      >-> decayingFilter 10
      >-> P.map (\(i, _) -> show i)
      >-> P.stdoutLn
    ] where

toFile :: MonadIO m => FilePath -> Consumer String m ()
toFile path = do
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  liftIO $ putStrLn $ "Writing to " ++path
  handle <- liftIO $ openFile path WriteMode
  () <- P.toHandle handle
  liftIO $ hClose handle
