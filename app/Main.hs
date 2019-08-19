module Main where

import           Parameters
import qualified Parameters.Weights as Weights
import qualified Parameters.Tree as Tree
import qualified Parameters.Requests as Requests
import qualified Parameters.Algorithm as Alg

import Evaluation

import Polysemy
import Polysemy.Trace
import Control.Monad
import System.IO
import Prelude
import System.Directory
import System.FilePath
import Conduit
import qualified Polysemy.Async as PA

main :: IO ()
main = runM
  . runTraceIO
  .@ PA.runAsyncInIO
  $ genArrow

createHandle :: FilePath -> IO Handle
createHandle path = do
  createDirectoryIfMissing True (takeDirectory path)
  liftIO $ putStrLn $ "Opening handle to " ++ path
  openFile path WriteMode

genArrow :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
genArrow = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 500
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    , Weights.unitEuclidian 2
    , Weights.unitEuclidian 3
    ]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "genArrow" par
    [ Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    , Alg.arrow Tree.random
    ]
    [ stretch, ratio, treeWeight ]

converges :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
converges = forM_ [ Parameters
    { randomSeed = 0
    , nodeCount = 100
    , requestCount = 100000
    , weights = w
    , requests = r
    }
  | w <-
    [ Weights.erdosRenyi (Weights.ErdosProbEpsilon 0)
    ]
  , r <-
    [ Requests.random
    ]
  ] $ \par -> runEvals "converges" par
    [ Alg.ivy Tree.mst
    , Alg.ivy Tree.shortPairs
    , Alg.ivy Tree.random
    , Alg.random Tree.mst
    , Alg.random Tree.shortPairs
    , Alg.random Tree.random
    ]
    [ ratio ]


testing :: Members '[Lift IO, Trace, PA.Async] r => Sem r ()
testing = runEvals "testing " params algs evals
  where
    params = Parameters
      { randomSeed = 0
      , nodeCount = 1000
      , requestCount = 100
      , weights = Weights.unitEuclidian 2
      , requests = Requests.random
      }
    algs = [ Alg.arrow Tree.mst
           , Alg.ivy Tree.mst
           , Alg.inbetweenWeighted 0.5 Tree.mst]
    evals = [stretch, ratio, treeWeight]
