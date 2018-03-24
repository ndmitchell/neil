{-# LANGUAGE RecordWildCards #-}

-- This script creates a binary distribution at dist/bin/$package-$ver.zip
-- or .tar.gz on Linux
module Binary(run) where

import Arguments
import Cabal hiding (run)
import Control.Monad.Extra
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra
import Data.List.Extra
import System.FilePath
import System.Directory.Extra


exe = if isWindows then "exe" else ""

copy src dest = do
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile src dest

run Binary{..} = Just $ withCurrentDirectory path $ withTempDir $ \tdir -> do
    src <- readCabal
    let ver = extractCabal "version" src
    let name = extractCabal "name" src
    let grab tag = concat [words $ drop (length tag + 1) x | x <- relines $ lines src, (tag ++ ":") `isPrefixOf` x]
    let dataDir = case grab "data-dir" of [] -> id; [x] -> (x </>)
    let files = grab "extra-doc-files" ++ map dataDir (grab "data-files")
    system_ $ "cabal sdist --output-directory=" ++ tdir
    let vname = name ++ "-" ++ ver
    let zname = if isWindows then vname ++ "-x86_64-windows.zip"
                else if isMac then vname ++ "-x86_64-osx.tar.gz"
                else vname ++ "-x86_64-linux.tar.gz"
    b <- withCurrentDirectory tdir $ do
        system_ "cabal install --dependencies"
        system_ "cabal configure --datadir=nul --disable-library-profiling"
        system_ "cabal build"
        let out = "bin" </> vname
        let built = "dist/build" </> name </> name <.> exe
        b <- doesFileExist built
        if not b then return False else do
            copy built (out </> name <.> exe)
            forM_ files $ \file -> when ('*' `notElem` file) $
                copy file $ out </> file
            withCurrentDirectory "bin" $
                if isWindows then
                    system_ $ "zip -9 -r " ++ zname ++ " " ++ vname
                else
                    system_ $ "tar -czvf " ++ zname ++ " " ++ vname
            return True
    if not b then
        putStrLn "Completed, but package does not have a similarly named binary"
     else do
        let res = "dist/bin" </> zname
        copy (tdir </> "bin" </> zname) res
        putStrLn $ "Completed, produced " ++ res
run _ = Nothing
