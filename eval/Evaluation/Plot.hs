module Evaluation.Plot where

import           Evaluation.Types

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Functor
import Data.Default.Class
import Graphics.Rendering.Chart.Easy
import System.Directory
import System.FilePath
import Data.Colour.Palette.ColorSet

plotResults :: FilePath -> EvalResults -> IO ()
plotResults path (EvalResults name results) = do

  let layouts = map toLayout results
      layouts' = layouts
        & ix 0.layout_title .~ name
        -- & _init.traverse.layout_legend .~ Nothing
      stacked = def
        & slayouts_layouts .~ map StackedLayout layouts'
        & slayouts_compress_legend .~ False
      width = 2000
      height = width `div` 2 * length layouts


  createDirectoryIfMissing True (takeDirectory path)
  void $ renderableToFile (FileOptions (width, height) PNG) path (toRenderable stacked)
  where

    lineColors :: [AlphaColour Double] = opaque <$> infiniteWebColors

    axisStyle :: AxisStyle = def
      & axis_label_style.font_size .~ 20

    toLayout :: (String, [(String, Series)]) -> Layout Double Double
    toLayout (title, series) = def
      & layout_y_axis.laxis_title .~ title
      & layout_plots .~ zipWith mkPlot lineColors series
      & layout_title_style.font_size .~ 50
      & layout_title_style.font_weight .~ FontWeightNormal
      & layout_x_axis.laxis_generate .~ autoScaledLogAxis
        (def @(LogAxisParams Double) & loga_labelf .~ map (show @Integer . floor))
      & layout_x_axis.laxis_title .~ "Requests"
      & layout_x_axis.laxis_title_style.font_size .~ 30
      & layout_x_axis.laxis_style .~ axisStyle
      & layout_y_axis.laxis_title_style.font_size .~ 40
      & layout_y_axis.laxis_style .~ axisStyle
      & layout_legend._Just.legend_label_style.font_size .~ 30
      & layout_margin .~ 40

    mkPlot color (n, ser) = toPlot $ def
      & plot_lines_title .~ n
      & plot_lines_values .~ [ser]
      & plot_lines_style.line_color .~ color
      & plot_lines_style.line_width .~ 3
