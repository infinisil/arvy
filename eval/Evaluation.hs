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
import Graphics.Rendering.Chart.Axis.Floating
import Control.Lens

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
  let layouts@(l:ls) = zipWith toLayout (map evalPlotDefaults evals) series
      stacked = def { _slayouts_layouts = map StackedLayout
                      (l { _layout_legend = Nothing } : ls) }
      path = "evals/weights:" ++ weightsId (weights params)
        ++ "/requests:" ++ requestsId (requests params)
        ++ ".png"
      width = 2000
      height = width `div` 2 * length layouts


  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  void $ sendM $ renderableToFile (FileOptions (width, height) PNG) path (toRenderable stacked)
  where
    runit :: AlgorithmParameter (RandomFu ': r) -> Sem r [Series]
    runit alg = runParams params alg (evalsConduit evals)

    lineColors :: [AlphaColour Double] = opaque <$>
      -- TODO: Automatically select colors, e.g. using palette library
      [purple, red, green, orange, pink, error "Not enough different colors defined"]

    axisStyle :: AxisStyle = def
      & axis_label_style.font_size .~ 20

    toLayout :: PlotDefaults -> [Series] -> Layout Double Double
    toLayout PlotDefaults { .. } series = plotDefaultLayout
      & layout_plots .~ zipWith3 mkPlot algs lineColors series
      & layout_title_style.font_size .~ 30
      & layout_title_style.font_weight .~ FontWeightNormal
      & layout_x_axis.laxis_generate .~ autoScaledLogAxis (def & loga_labelf .~ map (show . floor))
      & layout_x_axis.laxis_title .~ "Requests"
      & layout_x_axis.laxis_title_style.font_size .~ 30
      & layout_x_axis.laxis_style .~ axisStyle
      & layout_y_axis.laxis_title_style.font_size .~ 40
      & layout_y_axis.laxis_style .~ axisStyle
      & layout_legend._Just.legend_label_style.font_size .~ 30

      where

        mkPlot alg color serie = toPlot $ plotDefaultPlot
          & plot_lines_title .~ algorithmId alg
          & plot_lines_values .~ [serie]
          & plot_lines_style.line_color .~ color
          & plot_lines_style.line_width .~ 3



stretch :: Member (Lift IO) r => Eval (Sem r)
stretch = Eval
  { evalPlotDefaults = PlotDefaults
    { plotDefaultLayout = def
      { _layout_y_axis = def
        { _laxis_title = "Average tree stretch"
        }
      }
    , plotDefaultPlot = def
    }
  , evalFun = \env -> collectRequests (const ())
      .| enumerate
      .| logFilter env 100
      .| asConduit avgTreeStretchDiameter env
      .| C.map (\((i, _), (str, _)) -> (fromIntegral i, str))
  }


ratio :: Member (Lift IO) r => Eval (Sem r)
ratio = Eval
  { evalPlotDefaults = PlotDefaults
    { plotDefaultLayout = def
      { _layout_y_axis = def
        { _laxis_title = "Request ratio"
        }
      }
    , plotDefaultPlot = def
    }
  , evalFun = \env -> requestRatio env
      .| enumerate
      .| logFilter env 100
      .| C.map (\(i, rat) -> (fromIntegral i, rat))
  }
