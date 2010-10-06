
module Util where

import Control.Monad
import System.Directory
import System.Exit
import System.FilePath
import System.Directory
import Control.Exception as E
import System.IO
import System.Cmd
import Data.List
import Control.Concurrent


tempDir :: IO FilePath
tempDir = getTemporaryDirectory


withTempFile :: (FilePath -> IO a) -> IO a
withTempFile f = do
    tdir <- tempDir
    E.bracket
        (do (file,h) <- openTempFile "." "neilfile.tmp"; hClose h; return file)
        removeFile
        f

-- FIXME: does not clean up
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = do
    createDirectory "temp"
    f "temp"


cmdCodeOutErr :: String -> IO (ExitCode, String, String)
cmdCodeOutErr x = withTempFile $ \stderr -> withTempFile $ \stdout -> do
    res <- system $ x ++ " > " ++ stdout ++ " 2> " ++ stderr
    err <- readFile' stderr
    out <- readFile' stdout
    return (res,out,err)

cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Failed in system command: " ++ x

{-

cmdOut :: String -> IO String
cmdOut x = withTempFile $ \file -> do
    cmd $ x ++ " >> " ++ file
    readFile' file

cmdPassOut :: String -> IO (Bool,String)
cmdPassOut x = withTempFile $ \file -> do
    b <- cmdPass $ x ++ " >> " ++ file
    fmap ((,) b) $ readFile' file

cmdPass :: String -> IO Bool
cmdPass x = do
    res <- system x
    putStrLn $ x ++ " => " ++ show res
    return $ res == ExitSuccess
-}



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
