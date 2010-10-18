
module Cabal(run) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath
import Util
import Arguments


---------------------------------------------------------------------
-- COMMANDS

-- Policy: currently all must build flawlessly on 6.12.3, and at least build on 6.10.4
run :: Arguments -> Maybe (IO ())
run Sdist = Just $ do
    tested <- testedWith
    tested <- return $ if null tested then [""] else tested
    withTempDirectory $ \tdir -> do
        res <- cmdCode "cabal check"
        when (res /= ExitSuccess) $ error "Cabal check failed"
        cmd $ "cabal configure --builddir=" ++ tdir
        cmd $ "cabal sdist --builddir=" ++ tdir
        files <- getDirectoryContents tdir
        let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
        withDirectory tdir $ cmd $ "tar -xf " ++ tarball
        withDirectory (tdir </> dropExtension (dropExtension $ takeFileName tarball)) $ do
            cmd "cabal configure --ghc-option=-Werror --ghc-option=-fwarn-unused-imports --disable-library-profiling"
            cmd "cabal build"
            cmd "cabal haddock --executables"

            cmd "cabal clean"
            cmd "cabal configure --disable-library-profiling -w c:\\ghc\\ghc-6.10.4\\bin\\ghc.exe"
            cmd "cabal build"
        putStrLn $ "Ready to release!"

run Versions = Just $ error "Check to see what the permissable range is by repeatedly installing all the values in range"

run _ = Nothing



testedWith :: IO [String]
testedWith = do
    file <- findCabal
    case file of
        Nothing -> return []
        Just file -> do
            src <- readFile' file
            return $ concat [ map f $ words $ map (\x -> if x == ',' then ' ' else x) $ drop 12 x
                            | x <- lines src, "tested-with:" `isPrefixOf` x]

    where
        f x = map toLower a ++ "-" ++ drop 2 b
            where (a,b) = break (== '=') x


findCabal :: IO (Maybe FilePath)
findCabal = do
    x <- getDirectoryContents "."
    return $ listToMaybe $ filter ((==) ".cabal" . takeExtension) x
