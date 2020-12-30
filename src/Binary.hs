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
    system_ $ "cabal v1-sdist --output-directory=" ++ tdir
    let vname = name ++ "-" ++ ver
    let zname = if isWindows then vname ++ "-x86_64-windows.zip"
                else if isMac then vname ++ "-x86_64-osx.tar.gz"
                else vname ++ "-x86_64-linux.tar.gz"
    b <- withCurrentDirectory tdir $ do
        system_ "cabal v2-build --only-dependencies all"
        system_ "cabal v2-configure --datadir=nul --disable-library-profiling"
        system_ "cabal v2-build"
        let out = "bin" </> vname
        builtFiles <- listFilesRecursive "dist-newstyle/build"
        case find (\x -> takeFileName x == name <.> exe) builtFiles of
            Nothing -> pure False
            Just built -> do
                copy built (out </> name <.> exe)
                forM_ files $ \file -> when ('*' `notElem` file) $
                    copy file $ out </> file
                withCurrentDirectory "bin" $
                    if isWindows then
                        system_ $ "7z a " ++ zname ++ " " ++ vname
                    else
                        system_ $ "tar -czvf " ++ zname ++ " " ++ vname
                pure True
    if not b then
        putStrLn "Completed, but package does not have a similarly named binary"
     else do
        let res = "dist/bin" </> zname
        copy (tdir </> "bin" </> zname) res
        putStrLn $ "Completed, produced " ++ res
run _ = Nothing
