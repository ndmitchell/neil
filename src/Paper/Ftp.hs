
module Paper.Ftp(ftp) where

import Control.Monad
import System.Directory
import System.FilePath
import System.Process.Extra
import System.IO
import Paper.Util.IO


ftp :: FilePath -> IO ()
ftp darcs = do
    let name = takeFileName darcs
        out = darcs </> name <.> "patch"
        ftpfile = out <.> "ftp"

    b <- doesFileExist out
    when b $ removeFile out
    b <- doesFileExist (out <.> "send")
    when b $ removeFile (out <.> "send")

    system_ $ "darcs send" ++
                 " --repodir=\"" ++ darcs ++ "\"" ++
                 " --output=\"" ++ (out <.> "send") ++ "\""
    b <- doesFileExist (out <.> "send")
    when b $ do
        password <- getPassword
        writeFile ftpfile $ unlines
            ["ndm500"
            ,password
            ,"lcd " ++ darcs
            ,"cd web/patches"
            ,"binary"
            ,"put " ++ (name <.> "patch" <.> "send")
            ,"rename " ++ (name <.> "patch" <.> "send") ++ " " ++ (name <.> "patch")
            ,"get " ++ (name <.> "patch")
            ,"quit"]
        system_ $ "ftp -s:" ++ ftpfile ++ " ftp.york.ac.uk > nul"
        check out (out <.> "send")
        removeFile ftpfile
        removeFile out
        removeFile (out <.> "send")
        return ()


getPassword = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin NoBuffering
        hSetEcho stdout False
        hSetEcho stdin False
        putStr "Enter password: "
        s <- getLine
        putStrLn ""
        return s


check x y = do
    x1 <- readFile' x
    y1 <- readFile' y
    if x1 == y1
        then putStrLn "Success"
        else putStrLn "ERROR: FTP was unsuccessful"
