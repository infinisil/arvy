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
import Arvy.Algorithm


import qualified Data.Sequence as S
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
import Data.NonNull hiding (last)


data PlotDefaults = PlotDefaults
  { plotDefaultLayout :: Layout Double Double
  , plotDefaultPlot :: PlotLines Double Double
  }


--runEvals
--  :: forall r p a
--   . Members '[Lift IO, Trace, PA.Async] r
--  => String
--  -> Parameters p (RandomFu ': r)
--  -> [AlgorithmParameter p a (RandomFu ': r)]
--  -> [Eval (Sem (RandomFu ': r))]
--  -> Sem r ()
--runEvals key params@Parameters { .. } algs evals = do
--  asyncs <- mapM runit algs
--  series <- transpose <$> forM asyncs PA.await
--  let layouts = zipWith toLayout (map evalName evals) series
--      layouts' = layouts
--        & ix 0.layout_title .~ paramDescr params
--        -- & _init.traverse.layout_legend .~ Nothing
--      stacked = def
--        & slayouts_layouts .~ map StackedLayout layouts'
--        & slayouts_compress_legend .~ False
--      path = "evals/" ++ key ++ "/" ++ weightsId weights
--        ++ "-" ++ requestsId requests
--        ++ "-" ++ show nodeCount
--        ++ ".png"
--      width = 2000
--      height = width `div` 2 * length layouts
--
--
--  liftIO $ createDirectoryIfMissing True (takeDirectory path)
--  void $ sendM $ renderableToFile (FileOptions (width, height) PNG) path (toRenderable stacked)
--  where
--    runit :: AlgorithmParameter (RandomFu ': r) -> Sem r (Async [Series])
--    runit alg = fmap fromJust <$> runParams params alg (evalsConduit (algorithmId alg) evals)
--
--    lineColors :: [AlphaColour Double] = opaque <$> infiniteWebColors
--
--    axisStyle :: AxisStyle = def
--      & axis_label_style.font_size .~ 20
--
--    toLayout :: String -> [Series] -> Layout Double Double
--    toLayout title series = def
--      & layout_y_axis.laxis_title .~ title
--      & layout_plots .~ zipWith3 mkPlot algs lineColors series
--      & layout_title_style.font_size .~ 50
--      & layout_title_style.font_weight .~ FontWeightNormal
--      & layout_x_axis.laxis_generate .~ autoScaledLogAxis (def & loga_labelf .~ map (show . floor))
--      & layout_x_axis.laxis_title .~ "Requests"
--      & layout_x_axis.laxis_title_style.font_size .~ 30
--      & layout_x_axis.laxis_style .~ axisStyle
--      & layout_y_axis.laxis_title_style.font_size .~ 40
--      & layout_y_axis.laxis_style .~ axisStyle
--      & layout_legend._Just.legend_label_style.font_size .~ 30
--      & layout_margin .~ 40
--
--      where
--
--        mkPlot alg color serie = toPlot $ def
--          & plot_lines_title .~ algorithmId alg
--          & plot_lines_values .~ [serie]
--          & plot_lines_style.line_color .~ color
--          & plot_lines_style.line_width .~ 3

dataPoints :: Int
dataPoints = 100

allEvals :: Member (Lift IO) r => [ Eval r ]
allEvals = [ evalTreeDist, evalTreeRatio, evalTreeEdgeDist, evalRequestDist, evalRequestRatio, evalRequestHops ]

evalTreeDist :: Member (Lift IO) r => Eval r
evalTreeDist = Eval
  { evalName = "Average tree distance"
  , evalFun = \env@Env { envNodeCount } -> enumerate
      .| logFilter env dataPoints
      .| asConduit totalPairWeight env
      .| C.map (\((i, _), w) -> (fromIntegral i, w / fromIntegral (envNodeCount * (envNodeCount - 1) `div` 2)))
  }

evalTreeRatio :: Member (Lift IO) r => Eval r
evalTreeRatio = Eval
  { evalName = "Average tree ratio"
  , evalFun = \env -> enumerate
      .| logFilter env dataPoints
      .| asConduit avgTreeStretchDiameter env
      .| C.map (\((i, _), (str, _)) -> (fromIntegral i, str))
  }

evalTreeEdgeDist :: Member (Lift IO) r => Eval r
evalTreeEdgeDist = Eval
  { evalName = "Average tree edge distance"
  , evalFun = \env@Env { envNodeCount } -> enumerate
      .| logFilter env dataPoints
      .| asConduit totalTreeWeight env
      .| C.map (\((i, _), rat) -> (fromIntegral i, rat / fromIntegral (envNodeCount - 1)))
  }

evalRequestRatio :: Member (Lift IO) r => Eval r
evalRequestRatio = Eval
  { evalName = "Average request ratio"
  , evalFun = \env -> requestRatios env
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, rat) -> (fromIntegral i, rat))
  }

evalRequestDist :: Member (Lift IO) r => Eval r
evalRequestDist = Eval
  { evalName = "Average request distance"
  , evalFun = \env@Env { envWeights } -> requestDists env
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, w) -> (fromIntegral i, w))
  }

evalRequestHops :: Member (Lift IO) r => Eval r
evalRequestHops = Eval
  { evalName = "Average request hop count"
  , evalFun = \env -> hopCount
      .| C.map fromIntegral
      .| movingAverage True 100
      .| enumerate
      .| logFilter env dataPoints
      .| C.map (\(i, hops) -> (fromIntegral i, hops))
  }
