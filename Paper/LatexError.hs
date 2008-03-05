
module Paper.LatexError(latexError) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath


-- given a list of all the files that went into the mix
-- and the log file
-- and the errors reported to the screen
-- attempt to diagnose something
latexError :: [FilePath] -> FilePath -> String -> IO ()
latexError texFiles logFile messages = do
        putStrLn "\nAnalysing Errors:\n"
        src <- readFile logFile
        let log = processLog src
        mapM_ (f log) $ nub $ map (\(a,b,_) -> (a,b)) $ mapMaybe splitErrLine $ lines messages
        mapM_ checkFile texFiles
    where
        rep from to x = if x == from then to else x

        f log (file,line) = do
                mapM_ putStr $ take 1 [c | (a,b,c) <- log, a == file2, b == line]
                showSection file line
            where file2 = takeBaseName file

splitErrLine s
    | null rest2 || null pos || not (all isDigit pos) = Nothing
    | otherwise = Just (s1++file, read pos :: Int, drop 1 rest2)
    where
        (s1,s2) = splitAt 3 s
        (file,rest1) = break (== ':') s2
        (pos,rest2) = break (== ':') $ drop 1 rest1


-- show a section of a file, centered around a particular point
showSection :: FilePath -> Int -> IO ()
showSection file pos = do
        b <- doesFileExist file
        when b $ do
            src <- readFile file
            putStrLn $ unlines $ map f $ take 5 $ drop (pos - 3) $ zip [1..] $ lines src
    where f (p,s) = show p ++ (if p == pos then ">  " else "   ") ++ s



processLog :: String -> [(FilePath,Int,String)]
processLog x = f (lines x)
    where
        f [] = []
        f (x:xs) = case splitErrLine x of
                       Nothing -> f xs
                       Just (a,b,c) -> (takeBaseName a,b,unlines (c:d)) : f e
                           where (d,e) = break null xs


---------------------------------------------------------------------
-- Check a file for obvious bracketing errors
checkFile :: FilePath -> IO ()
checkFile file = do
        src <- readFile file
        let errs = f [] (zip [0..] $ lines src)
        putStr $ unlines ["Error in " ++ takeBaseName file ++ ":" ++ show n ++ ", " ++ x | (n,x) <- errs]
    where
        (open,shut) = unzip brackets
        search x ys = listToMaybe [(i, drop (length y) x) | (i,y) <- zip [0..] ys, y `isPrefixOf` x]

        f seen [] = if null seen then [] else
                    [(n, "Bracket openned but never shut " ++ show (open !! i)) | (n,i) <- seen]
        f seen ((n,[]):xs) = f seen xs
        f seen ((n,x):xs) =
                case (search x open, search x shut) of
                    (Just (i,x),_) -> f ((n,i):seen) ((n,x):xs)
                    (_,Just (i,x)) | null seen ->
                        [(n,"Close bracket " ++ show (shut !! i) ++ ", but no open anywhere")]
                                   | otherwise ->
                        let ((n2,i2):seen2) = seen
                        in if i2 == i then f seen2 ((n,x):xs)
                           else [(n,"Bracket mismatch, " ++ show (open !! i2) ++ " openned on " ++ show n2 ++
                                   " but shut with " ++ show (shut !! i))]
                    _ -> f seen ((n,tail x):xs)


brackets :: [(String,String)]
brackets = let (*) = (,) in
    ["{" * "}"
    ]
