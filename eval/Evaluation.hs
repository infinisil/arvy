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
import Graphics.Rendering.Chart.Easy
import Data.List
import Parameters
import Parameters.Weights
import Parameters.Requests
import Polysemy.RandomFu
import Polysemy.Trace
import System.Directory
import System.FilePath
import qualified Polysemy.Async as PA
import Control.Monad
import Data.Maybe
import Control.Concurrent.Async
import Data.Colour.Palette.ColorSet
import Data.Monoid
import Data.Array.IArray

type Series = [(Double, Double)]

data PlotDefaults = PlotDefaults
  { plotDefaultLayout :: Layout Double Double
  , plotDefaultPlot :: PlotLines Double Double
  }

data Eval m = Eval
  { evalName :: String
  , evalFun :: Env -> ConduitT ArvyEvent (Double, Double) m ()
  }

evalsConduit :: (Traversable f, Member Trace r) => String -> f (Eval (Sem r)) -> Env -> ConduitT ArvyEvent Void (Sem r) (f Series)
evalsConduit prefix evals env = --C.iterM (trace . show) .|
  sequenceConduits (fmap (\eval -> do
                             vals <- evalFun eval env .| C.sinkList
                             lift $ trace $ prefix ++ ", " ++ evalName eval ++ ": " ++ show (last vals)
                             return vals
                         ) evals)

runEvals
  :: forall r
   . Members '[Lift IO, Trace, PA.Async] r
  => String
  -> Parameters (RandomFu ': r)
  -> [AlgorithmParameter (RandomFu ': r)]
  -> [Eval (Sem (RandomFu ': r))]
  -> Sem r ()
runEvals key params@Parameters { .. } algs evals = do
  asyncs <- mapM runit algs
  series <- transpose <$> forM asyncs PA.await
  let layouts = zipWith toLayout (map evalName evals) series
      layouts' = layouts
        & ix 0.layout_title .~ paramDescr params
        -- & _init.traverse.layout_legend .~ Nothing
      stacked = def
        & slayouts_layouts .~ map StackedLayout layouts'
        & slayouts_compress_legend .~ False
      path = "evals/" ++ key ++ "/" ++ weightsId weights
        ++ "-" ++ requestsId requests
        ++ "-" ++ show nodeCount
        ++ ".png"
      width = 2000
      height = width `div` 2 * length layouts


  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  void $ sendM $ renderableToFile (FileOptions (width, height) PNG) path (toRenderable stacked)
  where
    runit :: AlgorithmParameter (RandomFu ': r) -> Sem r (Async [Series])
    runit alg = fmap fromJust <$> runParams params alg (evalsConduit (algorithmId alg) evals)

    lineColors :: [AlphaColour Double] = opaque <$> infiniteWebColors

    axisStyle :: AxisStyle = def
      & axis_label_style.font_size .~ 20

    toLayout :: String -> [Series] -> Layout Double Double
    toLayout title series = def
      & layout_y_axis.laxis_title .~ title
      & layout_plots .~ zipWith3 mkPlot algs lineColors series
      & layout_title_style.font_size .~ 50
      & layout_title_style.font_weight .~ FontWeightNormal
      & layout_x_axis.laxis_generate .~ autoScaledLogAxis (def & loga_labelf .~ map (show . floor))
      & layout_x_axis.laxis_title .~ "Requests"
      & layout_x_axis.laxis_title_style.font_size .~ 30
      & layout_x_axis.laxis_style .~ axisStyle
      & layout_y_axis.laxis_title_style.font_size .~ 40
      & layout_y_axis.laxis_style .~ axisStyle
      & layout_legend._Just.legend_label_style.font_size .~ 30
      & layout_margin .~ 40

      where

        mkPlot alg color serie = toPlot $ def
          & plot_lines_title .~ algorithmId alg
          & plot_lines_values .~ [serie]
          & plot_lines_style.line_color .~ color
          & plot_lines_style.line_width .~ 3



stretch :: Member (Lift IO) r => Eval (Sem r)
stretch = Eval
  { evalName = "Average tree stretch"
  , evalFun = \env -> collectRequests (const ())
      .| enumerate
      .| logFilter env 100
      .| asConduit avgTreeStretchDiameter env
      .| C.map (\((i, _), (str, _)) -> (fromIntegral i, str))
  }

  --{ evalPlotDefaults = PlotDefaults
  --  { plotDefaultLayout = def
  --    { _layout_y_axis = def
  --      { _laxis_title = "Request ratio"
  --      }
  --    }
  --  , plotDefaultPlot = def
  --  }

ratio :: Member (Lift IO) r => Eval (Sem r)
ratio = Eval
  { evalName = "Request ratio"
  , evalFun = \env -> requestRatio env
      .| meanStddev
      .| enumerate
      .| logFilter env 100
      .| C.map (\(i, (rat, _)) -> (fromIntegral i, rat))
  }

requestWeight :: Member (Lift IO) r => Eval (Sem r)
requestWeight = Eval
  { evalName = "Request weight"
  , evalFun = \env@Env { envWeights } -> collectRequests (\edge -> Sum (envWeights ! edge))
      .| C.map (\(Request _ _ (Sum path)) -> path)
      .| meanStddev
      .| enumerate
      .| logFilter env 100
      .| C.map (\(i, (w, _)) -> (fromIntegral i, w))
  }

treeWeight :: Member (Lift IO) r => Eval (Sem r)
treeWeight = Eval
  { evalName = "treeWeight"
  , evalFun = \env -> collectRequests (const ())
      .| enumerate
      .| logFilter env 100
      .| asConduit totalTreeWeight env
      .| C.map (\((i, _), rat) -> (fromIntegral i, rat))
  }

{-# INLINE requestHops #-}
requestHops :: Member (Lift IO) r => Eval (Sem r)
requestHops = Eval
  { evalName = "Request hops"
  , evalFun = \env -> hopCount @Double
      .| movingAverage True 100
      .| enumerate
      .| logFilter env 100
      .| C.map (\(i, hops) -> (fromIntegral i, hops))
  }
