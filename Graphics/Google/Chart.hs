-- GoogleChart -- a Haskell module for using Google's Chart API
-- Copyright (c) 2008 Evan Martin <martine@danga.com>

{-| This module is for generating web-based charts using Google's Chart API:
<http://code.google.com/apis/chart/>.  Its output is URLs that will resolve
to a PNG image of the resulting chart.

Most of the functions in this module, with names like @setFoo@, take a 'Chart'
as an argument and produce a new 'Chart' with the specified attribute added.
These calls are designed to be chained together.  See the example below.

'Chart's are represented as a hierarchy of type classes so that parameters
that only affect a specific chart type are only available to that chart type.

@
putStrLn \"URL for your chart:\"
putStrLn $ 'chartURL' $
  'setSize' 400 257 $
  'setTitle' \"My Chart\" $
  'setData' ('encodeDataSimple' [[1..20]]) $
  'setLegend' [\"1 to 20\"] $
  'newLineChart'
@

This produces:
<http://chart.apis.google.com/chart?chs=400x257&chtt=My+Chart&chd=s%3aBCDEFGHIJKLMNOPQRSTU&chdl=1+to+20&cht=lc>

Remaining features to implement:

- lxy line charts

- chbh bar charts

- scatter plots

- background\/fill colors

- all style attributes
-}

module Graphics.Google.Chart (
  -- * Chart basics
  -- | These functions and types are shared by all chart types.
  Chart,
  chartURL,
  setSize, setTitle, setTitleOpts, setData,
  -- ** Chart data
  -- | There are multiple options for encoding chart data.  See
  -- <http://code.google.com/apis/chart/#chart_data> for more details on
  -- the tradeoffs of the different encoding options.
  ChartData, encodeDataSimple, encodeDataText, encodeDataExtended,

  setDataColors,

  -- * Chart features
  -- ** Legends
  LegendChart, setLegend,
  -- ** Axis labels
  -- | The order of elements in the lists passed to these functions matter:
  -- If the first 'AxisType' passed to 'setAxisTypes' is 'AxisBottom', then
  -- the first set of labels passed to 'setAxisLabels' refers to that bottom
  -- axis.
  AxisLabelChart, setAxisTypes, AxisType(..), setAxisLabels,
  setAxisLabelPositions, setAxisRanges, AxisAlignment(..), setAxisStyles,

  -- * Specific chart types
  -- ** Line charts
  LineChart, newLineChart,

  -- ** Pie charts
  PieChart, newPieChart, PieStyle(..), setLabels,

  -- ** Bar charts
  BarChart, newBarChart, Orientation(..), BarStyle(..),

  -- ** Venn diagrams
  VennDiagram, newVennDiagram
) where

import Data.Char (chr, ord)
import Data.List (intersperse)
import Numeric (showHex)


-- Intercalcate is only available in 6.8 and above
-- in a while this line can be removed
intercalate a b = concat (intersperse a b)


-- | URL-encode a string.
urlEnc str = concatMap enc str where
  enc c | c >= 'A' && c <= 'Z' = [c]
        | c >= 'a' && c <= 'z' = [c]
        | c >= '0' && c <= '9' = [c]
        | c `elem` safe        = [c]
        | c == ' '             = "+"
        | otherwise  = '%':(showHex (ord c) "")
  -- Argh, different resources differ on which characters need escaping.
  -- This is likely wrong.
  safe = "$-_.!*'(),|:"

-- All charts are internally represented as a list of (key,value) pairs.
-- We could switch this to Data.Map if it matters.
type Params = [(String,String)]

-- |The type class underneath all Charts.
class Chart c where
  params :: c -> Params
  fromParams :: Params -> c

setParam :: (Chart c) => String -> String -> c -> c
setParam key val c = fromParams $ (key,val) : filter ((/= key) . fst) (params c)

-- |Set the width and height, in pixels, of the resulting image.
setSize :: (Chart c) => Int -> Int -> c -> c
setSize width height = setParam "chs" (show width ++ "x" ++ show height)

-- |Set the title of the chart.
setTitle :: (Chart c) => String -> c -> c
setTitle title = setParam "chtt" title

-- |Set options for the display of the title of the chart.
setTitleOpts :: (Chart c) => String -- ^Color of the text.
                          -> Int -- ^Size of the text.
                          -> c -> c
setTitleOpts color size = setParam "chts" (color ++ "," ++ show size)

-- |Set the data displayed by the chart.
setData :: (Chart c) => ChartData -> c -> c
setData (ChartData str) = setParam "chd" str

-- |Construct the URL used to show the chart.
chartURL :: (Chart c) => c -> String
chartURL chart = baseURL ++ intercalate "&" urlparams where
  baseURL = "http://chart.apis.google.com/chart?"
  urlparams = [urlEnc a ++ "=" ++ urlEnc b | (a,b) <- params chart]

-- |All the encoding methods produce 'ChartData', which is usable by 'setData'.
newtype ChartData = ChartData String deriving Show

-- |Encode data using the \"simple\" encoding, which maps each input value
-- to a single letter in the URL.  This produces minimal URLs but doesn't have
-- as lot of resolution.  Input values must be in the range @0 <= x <= 61@.
-- Values outside the valid input range will be considered missing data.
encodeDataSimple :: [[Int]] -> ChartData
encodeDataSimple datas =
  ChartData $ "s:" ++ intercalate "," (map (map enc) datas) where
  enc i | i >= 0  && i <= 25 = chr (ord 'A' + i)
        | i >= 26 && i <= 51 = chr (ord 'a' + (i - 26))
        | i >= 52 && i <= 61 = chr (ord '0' + (i - 52))
        | otherwise          = '_'

-- |Encode data using the \"text\" encoding, which maps each input value to
-- its string representation (e.g. \"3.4\") in the URL.  This has more
-- resolution than simple encoding but produces larger URLs.  Input values must
-- be in the range @0 <= x <= 100@.  Values outside the valid input range will
-- be considered missing data.  Values with more than one decimal place of
-- resolution will be rounded.
encodeDataText :: RealFrac a => [[a]] -> ChartData
-- I chose RealFrac because it's necessary for floor; what is appropriate here?
-- The input must effectively be fixed-point.
encodeDataText datas =
  ChartData $ "t:" ++ intercalate "|" (map encData datas) where
  encData = intercalate "," . map encDatum
  encDatum i | i >= 0 && i <= 100 = showDecimal i
             | otherwise          = "-1"
  showDecimal :: RealFrac a => a -> String
  showDecimal i = show (fromIntegral (round (i * 10.0)) / 10.0)

-- |Encode data using the \"extended\" encoding, which maps each input value
-- to a two-character pair in base 64.  This has more resolution than text
-- encoding and is more compact.  Input values must be in the range @0 <= x <=
-- 4095@.  Values outside the valid input range will be considered missing
-- data.
encodeDataExtended :: [[Int]] -> ChartData
encodeDataExtended datas =
  ChartData $ "e:" ++ intercalate "," (map (concatMap encDatum) datas) where
  encDatum i | i >= 0 && i < 4096 = let (a, b) = i `quotRem` 64 in
                                    [encChar a, encChar b]
             | otherwise          = "__"
  encChar i | i >= 0  && i <= 25 = chr (ord 'A' + i)
            | i >= 26 && i <= 51 = chr (ord 'a' + (i - 26))
            | i >= 52 && i <= 61 = chr (ord '0' + (i - 52))
            | i == 62            = '-'
            | i == 63            = '.'

-- |Set data set colors.  The nth color specified here colors the nth data
-- set in the 'ChartData' passed to 'setData'.  See
-- <http://code.google.com/apis/chart/#line_bar_pie_colors> for more
-- information.
setDataColors :: Chart c => [String] -> c -> c
setDataColors colors = setParam "chco" (intercalate "," colors)

-- |LegendChart represents charts that can display legends with 'setLegend'.
class Chart c => LegendChart c
-- |Set the legend for the corresponding data sets.  The colors are taken
-- from the data set colors.
setLegend :: LegendChart c => [String] -> c -> c
setLegend strs = setParam "chdl" (intercalate "|" strs)

-- |AxisLabelChart represents charts that can display axis labels.
class Chart c => AxisLabelChart c

-- |Where to display an axis.
data AxisType = AxisBottom | AxisTop | AxisLeft | AxisRight

-- |Set which axes to display.  Repeating an 'AxisType' produces multiple
-- sets of labels on that axis.
setAxisTypes :: AxisLabelChart c => [AxisType] -> c -> c
setAxisTypes axes = setParam "chxt" (intercalate "," (map axisChar axes)) where
  axisChar AxisBottom = "x"
  axisChar AxisTop    = "t"
  axisChar AxisLeft   = "y"
  axisChar AxisRight  = "r"

-- |Set axis labels.  The nth list of strings in the argument sets the labels
-- for the nth axis specified with 'setAxisTypes'.  An empty list of strings
-- skips labelling the corresponding axis.
setAxisLabels :: AxisLabelChart c => [[String]] -> c -> c
setAxisLabels axislabels =
  setParam "chxl" (intercalate "|" (zipWith axisLabel [0..] axislabels)) where
  axisLabel :: Int -> [String] -> String
  axisLabel _     []     = ""
  axisLabel index labels = show index ++ ":|" ++ intercalate "|" labels

-- |Set axis label positions.  The nth list of Ints in the argument sets the
-- positions for the nth axis specified with 'setAxisTypes'.  An empty list
-- skips setting position for the corresponding axis.
setAxisLabelPositions :: AxisLabelChart c => [[Int]] -> c -> c
setAxisLabelPositions positions =
  setParam "chxp" (intercalate "|" (zipWith axisPosn [0..] positions)) where
  axisPosn _     [] = ""
  axisPosn index xs = intercalate "," (map show (index:xs))

-- |Set axis ranges.  The nth pair of Ints in the argument sets the range
-- for the nth axis specified with 'setAxisTypes'.
setAxisRanges :: AxisLabelChart c => [(Int,Int)] -> c -> c
setAxisRanges ranges =
  setParam "chxr" (intercalate "|" (zipWith axisRange [0..] ranges)) where
  axisRange index (min,max) = intercalate "," (map show [index,min,max])

-- |Text alignment for labels on an axis.
data AxisAlignment = AlignLeft | AlignCenter | AlignRight

-- |Set axis styles.  The nth element in the argument sets the style for the
-- nth axis specified with 'setAxisTypes'.  Each style is a tuple of
-- (color, font size, text alignment).
setAxisStyles :: AxisLabelChart c => [(String,Int,AxisAlignment)] -> c -> c
setAxisStyles styles =
  setParam "chxs" (intercalate "|" (zipWith axisStyle [0..] styles)) where
  axisStyle index (color, size, align) =
    intercalate "," [show index, color, show size, alignString align]
  alignString AlignLeft   = "-1"
  alignString AlignCenter = "0"
  alignString AlignRight  = "1"


newtype LineChart = LineChart Params
instance Chart LineChart where
  params (LineChart p) = p
  fromParams = LineChart
instance LegendChart LineChart
instance AxisLabelChart LineChart

newLineChart :: LineChart
newLineChart = fromParams [("cht","lc")]

newtype PieChart = PieChart Params
instance Chart PieChart where
  params (PieChart p) = p
  fromParams = PieChart
data PieStyle = Pie2D | Pie3D
newPieChart :: PieStyle -> PieChart
newPieChart Pie2D = fromParams [("cht","p")]
newPieChart Pie3D = fromParams [("cht","p3")]

-- |Set labels for the different data points on the chart.
-- Specify missing values by passing an empty string.
setLabels :: [String] -> PieChart -> PieChart
setLabels labels = setParam "chl" $ intercalate "|" labels

newtype BarChart = BarChart Params
instance Chart BarChart where
  params (BarChart p) = p
  fromParams = BarChart
instance LegendChart BarChart
instance AxisLabelChart BarChart
data Orientation = Horizontal | Vertical
data BarStyle = Stacked | Grouped
newBarChart :: Orientation -> BarStyle -> BarChart
newBarChart orient style =
  fromParams [("cht",'b':oLetter orient:sLetter style:[])] where
  oLetter Horizontal = 'h'
  oLetter Vertical   = 'v'
  sLetter Stacked = 's'
  sLetter Grouped = 'g'

-- |Venn diagram data is specified in a particular format.  There should be
-- exactly seven data values, which represent, in order:
--   circle A size, circle B size, circle C size,
--   A\/B overlap, A\/C overlap, B\/C overlap,
--   A\/B\/C overlap.
newtype VennDiagram = VennDiagram Params
instance Chart VennDiagram where
  params (VennDiagram p) = p
  fromParams = VennDiagram
instance LegendChart VennDiagram
newVennDiagram :: VennDiagram
newVennDiagram = fromParams [("cht", "v")]
