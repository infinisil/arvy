module Evaluation.Types where

import           Arvy.Local
import           Data.Array.IO

data Env = Env
  { envNodeCount    :: NodeCount
  , envRequestCount :: Int
  , envWeights      :: GraphWeights
  , envTree         :: IOUArray Node Node
  }
