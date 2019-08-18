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
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Data.List
import Parameters
import Parameters.Weights
import Parameters.Requests
import Polysemy.RandomFu
import Polysemy.Trace
import System.Directory
import System.FilePath

type Series = [(Double, Double)]

data PlotDefaults = PlotDefaults
  { plotDefaultLayout :: Layout Double Double
  , plotDefaultPlot :: PlotLines Double Double
  }

data Eval m = Eval
  { evalPlotDefaults :: PlotDefaults
  , evalFun :: Env -> ConduitT ArvyEvent (Double, Double) m ()
  }

evalsConduit :: (Traversable f, Monad m) => f (Eval m) -> Env -> ConduitT ArvyEvent Void m (f Series)
evalsConduit evals env = sequenceConduits $ fmap (\eval -> evalFun eval env .| C.sinkList) evals


runEvals
  :: forall r
   . Members '[Lift IO, Trace] r
  => Parameters (RandomFu ': r)
  -> [AlgorithmParameter (RandomFu ': r)]
  -> [Eval (Sem (RandomFu ': r))]
  -> Sem r ()
runEvals params algs evals = do
  series <- transpose <$> mapM runit algs
  let layouts = zipWith toLayout (map evalPlotDefaults evals) series
      stacked = def { _slayouts_layouts = map StackedLayout layouts }
      path = "weights:" ++ weightsId (weights params)
        ++ "/requests:" ++ requestsId (requests params)
        ++ ".png"
      width = 1000
      height = width `div` 2 * length layouts

  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  void $ sendM $ renderableToFile (FileOptions (width, height) PNG) path (toRenderable stacked)
  where
    runit :: AlgorithmParameter (RandomFu ': r) -> Sem r [Series]
    runit alg = runParams params alg (evalsConduit evals)

    lineColors :: [AlphaColour Double] = opaque <$>
      -- TODO: Automatically select colors, e.g. using palette library
      [purple, red, green, orange, pink, error "Not enough different colors defined"]

    toLayout :: PlotDefaults -> [Series] -> Layout Double Double
    toLayout PlotDefaults { .. } series = plotDefaultLayout { _layout_plots = zipWith3 mkPlot algs lineColors series }
      where mkPlot alg color serie = toPlot plotDefaultPlot
              { _plot_lines_title = algorithmId alg
              , _plot_lines_values = [serie]
              , _plot_lines_style = (_plot_lines_style plotDefaultPlot)
                { _line_color = color
                , _line_width = 3
                }
              }

paramFile :: Parameters r -> FilePath
paramFile params = "evals/weights:" ++ weightsId (weights params) ++ "/requests:" ++ requestsId (requests params)

renderLayouts :: Members '[Trace, Lift IO] r => [Layout Double Double] -> Sem r ()
renderLayouts layouts = do
  let stacked = def
        { _slayouts_layouts = map StackedLayout layouts
        , _slayouts_compress_legend = False
        }
  _ <- sendM $ renderableToFile (FileOptions (1000, 1000) PNG) "plot.png" (toRenderable stacked)
  return ()

stretch :: Member (Lift IO) r => Eval (Sem r)
stretch = Eval
  { evalPlotDefaults = PlotDefaults
    { plotDefaultLayout = def
      { _layout_title = "Average tree stretch"
      }
    , plotDefaultPlot = def
    }
  , evalFun = \env -> collectRequests (const ())
      .| enumerate
      .| decayingFilter 1
      .| asConduit avgTreeStretchDiameter env
      .| C.map (\((i, _), (str, _)) -> (fromIntegral i, str))
  }


ratio :: Member (Lift IO) r => Eval (Sem r)
ratio = Eval
  { evalPlotDefaults = PlotDefaults
    { plotDefaultLayout = def
    , plotDefaultPlot = def
    }
  , evalFun = \env -> requestRatio env
      .| movingAverage True 100
      .| enumerate
      .| decayingFilter 1
      .| C.map (\(i, rat) -> (fromIntegral i, rat))
  }
