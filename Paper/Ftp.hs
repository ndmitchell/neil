
module Paper.Ftp(ftp) where

import Control.Monad
import System.Cmd
import System.Directory
import System.FilePath
import System.IO


ftp :: FilePath -> IO ()
ftp darcs = do
    let name = takeFileName darcs
        out = darcs </> name <.> "patch"
        ftpfile = out <.> "ftp"
    b <- doesFileExist out
    when b $ removeFile out

    system $ "darcs send" ++
                 " --repodir=\"" ++ darcs ++ "\"" ++
                 " --output=\"" ++ out ++ "\""
    b <- doesFileExist out
    when b $ do
        password <- getPassword
        writeFile ftpfile $ unlines
            ["ndm500"
            ,password
            ,"cd web/patches"
            ,"binary"
            ,"put " ++ (name <.> "patch")
            ,"quit"]
        system $ "ftp -s:" ++ ftpfile ++ " ftp.york.ac.uk"
        removeFile ftpfile
        removeFile out
        return ()


getPassword = do
        e <- hGetEcho stdin
        hSetEcho stdin False
        putStr "Enter password: "
        r <- f
        hSetEcho stdin e
        return r
    where
        f = do x <- getChar
               if x == '\n' then return [] else do
                   putStr "\b*"
                   xs <- f
                   return (x:xs)
