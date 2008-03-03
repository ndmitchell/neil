
module Main(main) where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

import Paper.WordCount


{-
COMMANDS:
make -- compile the document
wc -- word count
chart -- make a chart
spell -- complete spell check
colin -- auto-colin grammar check
haskell -- auto-haskell checking

CONVENTIONS:
all object files go in obj/ directory
all stored files go in paper/ directory
    the output from chart
    word count logs (used for charting)
    grammar checking logs
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:files) -> do
            files <- getFiles files
            process cmd files
        _ -> do process "" []


process :: String -> [FilePath] -> IO ()
process "wc" files = do
    let shw = fixed ("total" : map takeBaseName files)
    res <- flip mapM files $ \file -> do
        putStr $ shw (takeBaseName file) ++ "  "
        count <- wordCount file
        putStrLn $ int count
        return (file,count)
    putStrLn $ shw "Total" ++ "  " ++ int (sum $ map snd res)

process x files = putStrLn $ "Error: Unknown action, " ++ show x




getFiles :: [String] -> IO [FilePath]
getFiles [] = do
    d <- getCurrentDirectory
    getFiles [d]
getFiles x = liftM concat $ mapM (\x -> canonicalizePath x >>= getFile) x


getFile :: String -> IO [FilePath]
getFile x = do
    b <- doesDirectoryExist x
    if b then do
        s <- getDirectoryContents x
        return $ filter ((==) ".tex" . takeExtension) $ map (x </>) s
     else do
        b <- doesFileExist x
        if b then return [x] else error $ "File not found: " ++ x




----- mini formatting library

padR, padL :: Int -> String -> String
padL n s = replicate (n - length s) ' ' ++ s
padR n s = s ++ replicate (n - length s) ' '


maxIntWidth = maximum $ map (length . show) [minBound::Int, maxBound]

int :: Int -> String
int = padL maxIntWidth . show


fixed :: [String] -> String -> String
fixed ss = let n = maximum $ map length ss
           in \s -> padR n s

