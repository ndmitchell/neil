{-# LANGUAGE CPP #-}
module Paper.Main(main) where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

import Paper.Util.FileData
#ifdef NON_STACKAGE_DEPENDENCIES
import Paper.Graph
#endif
import Paper.Make
import Paper.Talk
import Paper.WordCount
import Paper.Haskell
import Paper.Haskell2.All
import Paper.Push
import Paper.Sync
import Paper.Ftp
import Paper.Ref
import Paper.Todo


{-
COMMANDS:
make -- compile the document
wc -- word count
wcmin -- minimal word count
graph -- make a graph
spell -- complete spell check
colin -- auto-colin grammar check
haskell -- auto-haskell checking

CONVENTIONS:
all object files go in obj/ directory
all stored files go in paper/ directory
    the output from chart
    word count logs (used for charting)
    grammar checking logs

all commands that take arguments take either:
    nothing - use the current directory
    a directory name - index.tex is the main file, all other *.tex files are extras
    a list of files - first is the main file, all others are extras (all must be the same directory)
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:files) -> do
            files <- getFileData files
            process cmd files
        _ -> error "No arguments given"


wc :: (FilePath -> IO Int) -> FileData -> IO ()
wc f files = do
    let shw = fixed ("total" : map dropExtension (allFiles files))
    res <- forM (allFiles files) $ \file -> do
        putStr $ shw (dropExtension file) ++ "  "
        count <- f (directory files </> file)
        putStrLn $ int count
        return (file,count)
    putStrLn $ shw "Total" ++ "  " ++ int (sum $ map snd res)
#ifdef NON_STACKAGE_DEPENDENCIES
    root <- settingsDir files
    graphLog (root </> "graph.txt") res
#endif

process :: String -> FileData -> IO ()
process "wc" files = wc wordCountNorm files
process "wcmin" files = wc wordCountMin files

#ifdef NON_STACKAGE_DEPENDENCIES
process "graph" files = do
    root <- settingsDir files
    let res = root </> "graph.png"
    graphCreate (root </> "graph.txt") res (allFiles files)
    putStrLn $ "Written graph, " ++ res
#endif

process "make" files = do
    dat <- dataDir
    obj <- objDir "make" files
    make dat obj (directory files) (mainFile files) (allFiles files)

process "haskell" files = do
    obj <- objDir "haskell" files
    haskell obj files

process "haskell2" files = do
    obj <- objDir "haskell2" files
    haskell2 obj (argFiles files)

process "talk" files = do
    tlk <- objDir "talk" files
    talk tlk (argFiles files) (flags files)

process "push" files = push (darcs files)

process "sync" files = do
    dat <- dataDir
    sync (takeDirectory $ takeDirectory dat)

process "ref" files = ref (argFiles files)

process "todo" files = todo (argFiles files)

process "ftp" files = ftp (darcs files)

process "check" files = do
    mapM_ (`process` files) ["ref","todo"]
    putStrLn "All checks succeeded"

process x files = putStrLn $ "Error: Unknown action, " ++ show x


----- utility stuff

-- the directory where data files (paper.bib/paper.fmt) live
dataDir :: IO FilePath
dataDir = do
    x <- findExecutable "paper"
    case x of
        Nothing -> error "Couldn't find the data directory"
        Just y -> return $ dropFileName y </> "data"

#ifdef NON_STACKAGE_DEPENDENCIES
settingsDir :: FileData -> IO FilePath
settingsDir = ensureDir "paper"
#endif

objDir :: String -> FileData -> IO String
objDir cmd = ensureDir ("obj" </> cmd)


ensureDir :: FilePath -> FileData -> IO FilePath
ensureDir name files = do
    let s = directory files </> name
    createDirectoryIfMissing True s
    return s


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
