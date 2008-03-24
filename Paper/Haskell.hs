
module Paper.Haskell where

import Data.Ix
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import Paper.FileData
import Paper.Haskell.Fragment
import Paper.Haskell.Check


haskell obj files = do
    let incFile = directory files </> "Include.hs"
    b <- doesFileExist incFile
    pre <- if b then readFile incFile else return ""

    src <- readFile (directory files </> mainFile files)
    let (debug,ranges) = partition (== "d") $ flags files
    checkFragments (not $ null debug)
                   (checkRange $ parseRanges ranges)
                   pre (parseFragments src)


-- ranges are one of:
--   -from..to
--   -from..          (where the .. is optional)

parseRanges :: [String] -> [(Int,Int)]
parseRanges [] = [(minBound,maxBound)]
parseRanges xs = map f xs
    where
        f x = (read a, if null c then maxBound else read c)
            where
                (a,b) = span isDigit x
                c = dropWhile (== '.') b

-- a list of ranges, union'ed together
checkRange :: [(Int,Int)] -> Int -> Bool
checkRange []     i = False
checkRange (x:xs) i = inRange x i || checkRange xs i


rep from with = map (\x -> if x == from then with else x)
