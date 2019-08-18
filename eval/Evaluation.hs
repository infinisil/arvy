module Evaluation
  ( module Evaluation.Tree
  , module Evaluation.Utils
  , module Evaluation.Request
  , module Evaluation.Types
  , module Evaluation.Weights
  , module Evaluation
  ) where

import           Evaluation.Request
import           Evaluation.Tree
import           Evaluation.Types
import           Evaluation.Utils
import           Evaluation.Weights


import Graphics.Rendering.Chart
import Parameters.Algorithm
import Conduit
import qualified Data.Conduit.Combinators as C
import Graphics.Rendering.Chart.Backend.Cairo
import Arvy.Local
import Polysemy
import Data.Functor

type Series = [(Double, Double)]

data PlotDefaults = PlotDefaults
  { plotDefaultLayout :: Layout Double Double
  , plotDefaultPlot :: forall r . AlgorithmParameter r -> PlotLines Double Double
  }

data Eval m = Eval
  { evalPlotDefaults :: PlotDefaults
  , evalFun :: Env -> ConduitT ArvyEvent (Double, Double) m ()
  }

evalsConduit :: (Traversable f, Monad m) => f (Eval m) -> Env -> ConduitT ArvyEvent Void m (f Series)
evalsConduit evals env = sequenceConduits $ fmap (\eval -> evalFun eval env .| C.sinkList) evals

render :: Member (Lift IO) r => Layout Double Double -> Sem r ()
render layout = void $ sendM $ renderableToFile (FileOptions (2000, 2000) PNG) "plot.png" (toRenderable layout)
