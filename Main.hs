
module Main(main) where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

-- import Paper.WordCount


{-
COMMANDS:
make -- compile the document
wc -- word count
chart -- make a chart
spell -- complete spell check
colin -- auto-colin grammar check
haskell -- auto-haskell checking
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
    --count <- wordCount file
    --logCount file count
    let count = 1
    putStrLn $ show count ++ " words"

process x files = putStrLn $ "Error: Unknown action, " ++ show x




getFiles :: [String] -> IO [FilePath]
getFiles [] = do
    d <- getCurrentDirectory
    getFiles [d]
getFiles x = liftM concat $ mapM getFile x


getFile :: String -> IO [FilePath]
getFile x = do
    b <- doesDirectoryExist x
    if b then do
        s <- getDirectoryContents x
        return $ filter ((==) ".tex" . takeExtension) $ map (x </>) s
     else do
        b <- doesFileExist x
        if b then return [x] else error $ "File not found: " ++ x

