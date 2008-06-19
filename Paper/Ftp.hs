
module Paper.Ftp(ftp) where

import Control.Monad
import System.Cmd
import System.Directory
import System.FilePath


ftp :: FilePath -> IO ()
ftp darcs = do
    let name = takeFileName darcs
        out = darcs </> name <.> "patch"
    b <- doesFileExist out
    when b $ removeFile out

    system $ "darcs send" ++
                 " --repodir=\"" ++ darcs ++ "\"" ++
                 " --output=\"" ++ out ++ "\""
    b <- doesFileExist out
    when b $ do
        print "push it"
