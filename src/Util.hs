
module Util where

import Control.Monad
import System.Directory
import System.Exit
import Control.Exception as E
import System.IO
import System.Cmd
import Data.List
import Control.Concurrent


tempDir :: IO FilePath
tempDir = getTemporaryDirectory


withTempFile :: (FilePath -> IO a) -> IO a
withTempFile f = E.bracket
    (do (file,h) <- openTempFile "." "neil.tmp"; hClose h; return file)
    removeFile
    f


withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory f = E.bracket
    (do (file,h) <- openTempFile "." "neil.tmp"; hClose h; removeFile file; createDirectory file; return file)
    removeDirectoryRecursive
    f


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


cmdCodeOutErr :: String -> IO (ExitCode, String, String)
cmdCodeOutErr x = withTempFile $ \stderr -> withTempFile $ \stdout -> do
    res <- system $ x ++ " > " ++ stdout ++ " 2> " ++ stderr
    err <- readFile' stderr
    out <- readFile' stdout
    return (res,out,err)


cmdCode :: String -> IO ExitCode
cmdCode = system


cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Failed in system command: " ++ x


readFile' :: FilePath -> IO String
readFile' file = do
    h <- openFile file ReadMode
    src <- hGetContents h
    length src `seq` hClose h
    return src


liner :: IO (String -> IO ())
liner = do
    mvar <- newMVar 0
    return $ \msg -> modifyMVar_ mvar $ \old -> do
        let eol = "\n" `isSuffixOf` msg
        msg <- return $ takeWhile (/= '\n') msg
        let new = length msg
        putStr $ (if old == 0 then "" else replicate 1000 '\b') ++ msg ++ replicate (old - new) ' '
        if eol then putStrLn "" >> return 0 else return new
