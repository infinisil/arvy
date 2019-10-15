module Evaluation.Types where

import           Arvy.Algorithm
import           Conduit
import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.NonNull
import qualified Data.Sequence      as S
import           Data.Text          (Text)
import           Polysemy

type Edge = (Node, Node)
type GraphWeights = UArray Edge Weight

data Env = Env
  { envNodeCount    :: NodeCount
  , envRequestCount :: Int
  , envWeights      :: GraphWeights
  , envTree         :: IOUArray Node Node
  }

type Series = [(Double, Double)]

data Eval r = Eval
  { evalName :: Text
  , evalFun  :: Env -> ConduitT (NonNull (S.Seq Node)) (Double, Double) (Sem r) ()
  }

data EvalResults = EvalResults
  { evalResultsName :: Text
  , evalResults     :: [(Text, [(Text, Series)])]
  }
