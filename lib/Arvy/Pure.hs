module Arvy.Pure where

import           Arvy.Algorithm
import           Arvy.Local
import           Polysemy
import           Polysemy.Trace
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Conduit

runArvyLocalPure :: forall st . GraphWeights -> [Node] -> [st] -> (forall s . Arvy st '[Trace, Lift (ST s)]) -> [Node] -> (([String], [ArvyEvent]), [Node])
runArvyLocalPure weights initialTree initialStates algorithm requests = runST runArvyLocalPure' where
  runArvyLocalPure' :: forall s . ST s (([String], [ArvyEvent]), [Node])
  runArvyLocalPure' = do
    mutableTree :: STUArray s Node Node <- newListArray (0, length initialTree - 1) initialTree
    mutableStates :: STArray s Node st <- newListArray (0, length initialStates - 1) initialStates
    res <- runM $ runTraceAsList $ runConduit (
      traverse_ yield requests
      .| runArvyLocal weights mutableTree mutableStates algorithm
      .| sinkList)
    finalTree <- getElems mutableTree
    return (res, finalTree)

runArvyLocalPureStateless :: GraphWeights -> [Node] -> (forall s . Arvy () '[Trace, Lift (ST s)]) -> [Node] -> (([String], [ArvyEvent]), [Node])
runArvyLocalPureStateless weights initialTree = runArvyLocalPure weights initialTree (map (const ()) initialTree)
