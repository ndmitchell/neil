
module Util(cmd, cmdCode, cmdOut, cmdCodeOutErr) where

import Control.Monad
import System.Exit
import System.IO.Extra
import System.Cmd


cmdCodeOutErr :: String -> IO (ExitCode, String, String)
cmdCodeOutErr x = withTempFile $ \stderr -> withTempFile $ \stdout -> do
    res <- system $ x ++ " > " ++ stdout ++ " 2> " ++ stderr
    err <- readFile' stderr
    out <- readFile' stdout
    return (res,out,err)

cmdOut :: String -> IO String
cmdOut x = withTempFile $ \stdout -> do
    res <- system $ x ++ " > " ++ stdout
    out <- readFile' stdout
    when (res /= ExitSuccess) $
        error $ "Failed in system command: " ++ x
    return out


cmdCode :: String -> IO ExitCode
cmdCode = system


cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Failed in system command: " ++ x
