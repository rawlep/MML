module TestDisplay where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)
import Data.Time.Calendar
import Data.Time.LocalTime
---import Data.Colour.Names
--import Data.Colour
--import Data.Accessor
--import Graphics.Rendering.Chart
import Prices

------------------- example 11 ----------------------------------------------------------------------
chart borders = toRenderable layout
 where
  layout =
        layout1_title ^= "Sample Bars" ++ btitle
      $ layout1_title_style ^: font_size ^= 10
      $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
      $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
      $ layout1_plots ^= [ Left (plotBars bars2) ]
      $ defaultLayout1 :: Layout1 PlotIndex Double

  bars2 = plot_bars_titles ^= ["Cash","Equity"]
      $ plot_bars_values ^= addIndexes [[20,45],[45,30],[30,20],[70,25]]
      $ plot_bars_style ^= BarsClustered
      $ plot_bars_spacing ^= BarsFixGap 30 5
      $ plot_bars_item_styles ^= map mkstyle (cycle defaultColorSeq)
      $ defaultPlotBars

  alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart True) 320 240 "test9_small.png"
main1 ["big"]    = renderableToPNGFile (chart True) 800 600 "test9_big.png"
main1  _         = renderableToPNGFile (chart True) 320 240 "test9_small.png"

main = getArgs >>= main1
----------------------------------------------------------------------------------------------------
-----

date dd mm yyyy = (LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight)
ok y = (y == 2005) || (y == 2006)

getYear  = ffst . toGregorian . localDay
    where ffst (a,_,_) = a

chart1 = layout
  where

    price1 = plot_lines_style .> line_color ^= opaque blue
           $ plot_lines_values ^= [[ (d , v) | (d,v,_) <- prices, ok (getYear d) ]] --- [[ ((date d m y), v) | (d,m,y,v,_) <- prices, ]]
           $ plot_lines_title ^= "price 1"
           $ defaultPlotLines

    price2 = plot_lines_style .> line_color ^= opaque green
           $ plot_lines_values ^= [[ (d, v) | (d,_,v) <- prices, ok (getYear d) ]] --- [[ ((date d m y), v) | (d,m,y,_,v) <- prices, ]]
           $ plot_lines_title ^= "price 2"
           $ defaultPlotLines

    layout = layout1_title ^="Price History"
           $ layout1_left_axis ^: laxis_override ^= axisGridHide
           $ layout1_right_axis ^: laxis_override ^= axisGridHide
           $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
           $ layout1_plots ^= [Left (toPlot price1),
                               Right (toPlot price2)]
           $ layout1_grid_last ^= False
           $ defaultLayout1

main2 = do
    renderableToWindow (toRenderable chart1) 640 480
    renderableToPNGFile (toRenderable chart1) 640 480 "test2.png"
