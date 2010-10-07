
module Cabal(run) where

import Control.Monad
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Util
import Arguments


---------------------------------------------------------------------
-- COMMANDS

run :: Arguments -> Maybe (IO ())
run Sdist = Just $ withTempDirectory $ \tdir -> do
    res <- cmdCode "cabal check"
    when (res /= ExitSuccess) $ error "Cabal check failed"
    cmd $ "cabal configure --builddir=" ++ tdir
    cmd $ "cabal sdist --builddir=" ++ tdir
    files <- getDirectoryContents tdir
    let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withDirectory tdir $ cmd $ "tar -xf " ++ tarball
    withDirectory (tdir </> dropExtension (dropExtension $ takeFileName tarball)) $ do
        cmd "cabal configure --ghc-option=-Werror --ghc-option=-fwarn-unused-imports"
        cmd "cabal build"
        cmd "cabal haddock --executables"
    putStrLn "Ready to release!"

run _ = Nothing
