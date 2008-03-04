
module Paper.Graph(graphLog, graphCreate) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Ord
import System.Cmd
import System.Directory
import System.FilePath
import System.Time

import Graphics.Google.Chart


graphDir = "paper"
graphFile = graphDir </> "graph.txt"


graphLog :: [(FilePath,Int)] -> IO ()
graphLog xs = do
    t <- getClockTime
    t <- toCalendarTime t
    mapM_ f $ group [(l, show (t,r,b)) | (a,b) <- xs, let (l,r) = splitFileName a]
    where
        f xs = do
            let dir = fst $ head xs
                txt = map snd xs
            createDirectoryIfMissing True graphDir
            appendFile (dir </> graphFile) (unlines txt)


-- load the data
graphLoad :: [FilePath] -> IO [(Date,FilePath,Int)]
graphLoad xs =
    liftM (sortBy (comparing fst3) . concat) $ mapM f $
        group $ map splitFileName xs
    where
        f xs = do
            let dir = fst (head xs)
                files = map snd xs
            src <- readFile $ dir </> graphFile
            return [(date a,b,c) | s <- lines src, not $ null s, let (a,b,c) = read s, b `elem` files]

fst3 (x,_,_) = x

-- year, month (1 based), day (1 based)
type Date = (Int,Int,Int)

date :: CalendarTime -> Date
date t = (ctYear t, fromEnum (ctMonth t) + 1, ctDay t)


incDate :: Date -> Date
incDate (a,12,31) = (a+1,1,1)
incDate (a, b,31) = (a,b+1,1)
incDate (a, b, c) = (a,b,c+1)


totalCounts :: [FilePath] -> [(Date,FilePath,Int)] -> [(Date,Int)]
totalCounts _ [] = [((2000,1,1),0)]
totalCounts files xs = f (Map.fromList (zip files $ repeat 0)) xs steps
    where
        steps = iterate incDate $ fst3 (head xs)

        f now xxs@((a,b,c):xs) (s:ss)
            | a > s = dump s now : f now xxs ss
            | otherwise = f (Map.insert b c now) xs (s:ss)
        f now [] (s:ss) = [dump s now]

        dump s mp = (s, sum $ Map.elems mp)


graphCreate :: [FilePath] -> FilePath -> IO ()
graphCreate files dest = do
    xs <- graphLoad files
    let url = graphUrl $ totalCounts files xs
    system $ "wget \"" ++ url ++ "\" -O \"" ++ dest ++ "\""
    return ()


-- a list of date/count pairs
-- all the dates must be in order
graphUrl :: [(Date,Int)] -> String
graphUrl dat = chartURL $
        setSize 400 300 $
        setTitle "Word Count" $
        setAxisTypes [AxisBottom,AxisLeft] $
        setAxisLabels [xAxis, yAxis] $
        setData (encodeDataSimple [map scale allY]) $
        newLineChart
    where
        -- calculate the bounding box
        minX = let (a,b,c) = fst (head dat) in (a,b,1)
        maxX = let (a,b,c) = fst (last dat) in incDate (a,b,31)
        allY = if length dat == 1 then replicate 2 $ snd $ head dat else map snd dat
        minY = (minimum allY `div` 1000) * 1000
        maxY = (maximum allY `div` 1000) * 1000 + 1000

        -- calculate the axis
        yAxis = map show [minY, minY+1000 .. maxY]
        xAxis = map g $ takeWhile (<= maxX) $ iterate f minX
            where
                f (a,b,c) = incDate (a,b,31)
                g (a,b,c) = take 3 (show (toEnum (b-1) :: Month)) ++ " " ++ show (a `mod` 100)

        -- scaling (new range is 0 <= x <= 61)
        scale y = (61 * (y - minY)) `div` (maxY - minY)
