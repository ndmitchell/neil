
module Paper.Graph(graphLog) where

import Data.List
import System.Directory
import System.FilePath
import System.Time


graphLog :: [(FilePath,Int)] -> IO ()
graphLog xs = do
    t <- getClockTime
    t <- toCalendarTime t
    mapM_ f $ group [(l, show (t,r,b)) | (a,b) <- xs, let (l,r) = splitFileName a]
    where
        f xs = do
            let dir = fst $ head xs
                txt = map snd xs
            createDirectoryIfMissing True (dir </> "paper")
            appendFile (dir </> "paper" </> "graph.txt") (unlines txt)

