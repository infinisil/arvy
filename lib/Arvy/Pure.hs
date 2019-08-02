module Arvy.Pure where

import           Arvy.Algorithm
import           Arvy.Local
import           Pipes
import           Polysemy
import           Polysemy.Trace
import Control.Monad.ST
import Data.Array.ST
import qualified Pipes.Prelude as P
import Data.Foldable

runArvyLocalPure :: forall st . GraphWeights -> [Node] -> [st] -> (forall s . Arvy st '[Trace, Lift (ST s)]) -> [Node] -> [ArvyEvent]
runArvyLocalPure weights initialTree initialStates algorithm requests = runST runArvyLocalPure' where
  runArvyLocalPure' :: forall s . ST s [ArvyEvent]
  runArvyLocalPure' = do
    mutableTree :: STUArray s Node Node <- newListArray (0, length initialTree - 1) initialTree
    mutableStates :: STArray s Node st <- newListArray (0, length initialStates - 1) initialStates
    fmap snd $ runM $ runTraceAsList $ P.toListM (
      traverse_ yield requests
      >-> runArvyLocal weights mutableTree mutableStates algorithm)

runArvyLocalPureStateless :: GraphWeights -> [Node] -> (forall s . Arvy () '[Trace, Lift (ST s)]) -> [Node] -> [ArvyEvent]
runArvyLocalPureStateless weights initialTree = runArvyLocalPure weights initialTree (map (const ()) initialTree)
