{-# LANGUAGE OverloadedStrings #-}

module Evaluation.Plot where

import           Evaluation.Types

import           Control.Monad
import           Data.Colour.Palette.ColorSet
import           Data.Default.Class
import           Data.Functor
import           Data.List
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TIO
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Directory
import           System.FilePath

writeSeriesToDat :: FilePath -> [(Text, Series)] -> IO ()
writeSeriesToDat path allSeries@((_, firstSeries):_) = do
  let xs = map fst firstSeries
      labels = map fst allSeries
      yss = map (map snd . snd) allSeries
      firstLine = "x\t" <> T.intercalate "\t" labels
      dataLine x ys = T.pack (show x) <> "\t" <> T.intercalate "\t" (map (T.pack . show) ys)
      dataLines = zipWith dataLine xs (transpose yss)
      contents = T.unlines (firstLine : dataLines)

  TIO.writeFile path contents

writeResultsToDats :: Text -> EvalResults -> IO ()
writeResultsToDats name (EvalResults _ results) = do
  createDirectoryIfMissing True ("data/" <> T.unpack name)
  forM_ results $ \(sname, series) ->
    writeSeriesToDat (T.unpack ("data/" <> name <> "/" <> sname <> ".dat")) series

plotResults :: FilePath -> EvalResults -> IO ()
plotResults path (EvalResults name results) = do

  let layouts = map toLayout results
      layouts' = layouts
        & ix 0.layout_title .~ T.unpack name
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

    toLayout :: (Text, [(Text, Series)]) -> Layout Double Double
    toLayout (title, series) = def
      & layout_y_axis.laxis_title .~ T.unpack title
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
      & plot_lines_title .~ T.unpack n
      & plot_lines_values .~ [ser]
      & plot_lines_style.line_color .~ color
      & plot_lines_style.line_width .~ 3
