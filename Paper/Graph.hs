
module Paper.Graph(graphLog) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Ord
import System.Directory
import System.FilePath
import System.Time


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
graphLoad :: [FilePath] -> IO [(TimeStep,FilePath,Int)]
graphLoad xs =
    liftM (sortBy (comparing fst3) . concat) $ mapM f $
        group $ map splitFileName xs
    where
        f xs = do
            let dir = fst (head xs)
                files = map snd xs
            src <- readFile $ dir </> graphFile
            return [(timeStep a,b,c) | s <- lines src, not $ null s, let (a,b,c) = read s, b `elem` files]

fst3 (x,_,_) = x

type TimeStep = (Int,Int,Int)

timeStep :: CalendarTime -> TimeStep
timeStep t = (ctYear t, fromEnum (ctMonth t) + 1, ctDay t)


incStep :: TimeStep -> TimeStep
incStep (a,12,31) = (a+1,1,1)
incStep (a, b,31) = (a,b+1,1)
incStep (a, b, c) = (a,b,c+1)


totalCounts :: [FilePath] -> [(TimeStep,FilePath,Int)] -> [(TimeStep,Int)]
totalCounts _ [] = [((2000,1,1),0)]
totalCounts files xs = f (Map.fromList (zip files $ repeat 0)) xs steps
    where
        steps = iterate incStep $ fst3 (head xs)

        f now xxs@((a,b,c):xs) (s:ss)
            | a > s = dump s now : f now xxs ss
            | otherwise = f (Map.insert b c now) xs (s:ss)
        f now [] (s:ss) = [dump s now]

        dump s mp = (s, sum $ Map.elems mp)


graphCreate :: [FilePath] -> IO ()
graphCreate files = do
    xs <- graphLoad files
    xs <- return $ totalCounts files xs
    return ()
