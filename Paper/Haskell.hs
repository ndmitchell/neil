
module Paper.Haskell where

import Data.Ix
import Data.Char
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
    checkFragments ("-d" `elem` flags files)
                   (const True)
                   pre (parseFragments src)



-- range,range,range
-- 1..  ,3    ,
parseRange :: String -> [(Int,Int)]
parseRange "" = [(minBound,maxBound)]
parseRange xs = map f $ words $ rep ',' ' ' xs
    where
        f x = (fst,snd) 
            where
                fst = read a
                snd | null b = fst
                    | null c = maxBound
                    | otherwise = read c
                (a,b) = span isDigit x
                c = dropWhile (== '.') b

checkRange :: [(Int,Int)] -> Int -> Bool
checkRange []     i = False
checkRange (x:xs) i = inRange x i || checkRange xs i


rep from with = map (\x -> if x == from then with else x)
