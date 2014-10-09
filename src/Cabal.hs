{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Cabal(run, readCabal) where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Functor
import System.Directory.Extra
import System.IO.Extra
import System.FilePath
import Util hiding (readFile')
import Arguments

defAllow = ["7.0.4","7.2.2","7.4.2","7.6.3","7.8.2"]


---------------------------------------------------------------------
-- COMMANDS


-- | Check the .cabal file is well formed
cabalCheck :: IO ()
cabalCheck = do
    res <- cmdCode "cabal check"
    checkCabalFile
    checkReadme
    let require = ":set -fwarn-unused-binds -fwarn-unused-imports"
    src <- readFile' ".ghci"
    when (require `notElem` lines src) $
        error $ "The .ghci file does not contain " ++ require

-- | Run some commands in a temporary directory with the unpacked cabal
withSDist :: IO a -> IO a
withSDist run = withTempDir $ \tdir -> do
    cmd $ "cabal configure --builddir=" ++ tdir
    cmd $ "cabal sdist --builddir=" ++ tdir
    files <- getDirectoryContents tdir
    let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withCurrentDirectory tdir $ cmd $ "tar -xf " ++ tarball
    lst <- getDirectoryContentsRecursive tdir
    let binary = [".png",".gz",""]
    bad <- flip filterM lst $ \file ->
        return (takeExtension file `notElem` binary) &&^
        fmap ('\r' `elem`) (readFileBinary' file)
    when (bad /= []) $ do
        error $ unlines $ "The following files have \\r characters in, Windows newlines?" : bad
    withCurrentDirectory (tdir </> dropExtension (dropExtension $ takeFileName tarball)) run


run :: Arguments -> Maybe (IO ())
run Test{..} = Just $ do
    cabalCheck
    withSDist $ do
        cmd "cabal install --only-dependencies"
        cmd $ "cabal configure --enable-tests --disable-library-profiling " ++
              "--ghc-option=-fwarn-unused-binds --ghc-option=-fwarn-unused-imports " ++
              "--ghc-option=-Werror --ghc-option=-fno-warn-warnings-deprecations" -- CABAL BUG WORKAROUND :(
        cmd "cabal build"
        cmd "cabal test --show-details=always"
        when install $
            cmd "cabal install --force-reinstalls"

run Check = Just cabalCheck

run Sdist{..} = Just $ do
    cabalCheck
    tested <- testedWith
    withSDist $ do
        forM_ (sort tested) $ \x -> do -- deliberately start with the oldest first
            putStrLn $ "Building with " ++ x
            cmd "cabal clean"
            cmd $ "cabal install --only-dependencies " ++
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            cmd $ "cabal configure --ghc-option=-fwarn-unused-imports --disable-library-profiling " ++
                  "--ghc-option=-Werror --ghc-option=-fno-warn-warnings-deprecations " ++ -- CABAL BUG WORKAROUND :(
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            cmd "cabal build"
            cmd "cabal haddock --executables"
    cmd "cabal sdist"
    putStrLn $ "Ready to release! (remember to neil tag after uploading)"

run Docs = Just $ do
    src <- readCabal
    let [ver] = [strip $ drop 8 x | x <- lines src, "version:" `isPrefixOf` x]
    let [name] = [strip $ drop 5 x | x <- lines src, "name:" `isPrefixOf` x]
    cmd $ "cabal haddock --hoogle --hyperlink-source " ++
          "--html-location=http://hackage.haskell.org/package/" ++ name ++ "/docs " ++
          "--contents-location='http://hackage.haskell.org/package/" ++ name
    withTempDir $ \dir -> do
        cmd $ "cp -R dist/doc/html/" ++ name ++ " \"" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs"
        cmd $ "tar cvz -C " ++ dir ++ " --format=ustar -f " ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz " ++ name ++ "-" ++ ver ++ "-docs"
        cmd $ "curl -X PUT -H \"Content-Type: application/x-tar\" " ++
              "-H \"Content-Encoding: gzip\" " ++
              "-u NeilMitchell " ++
              "--data-binary \"@" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz\" " ++
              "https://hackage.haskell.org/package/" ++ name ++ "-" ++ ver ++ "/docs"

run _ = Nothing



testedWith :: IO [String]
testedWith = do
    src <- readCabal
    return $ concat [ map f $ words $ map (\x -> if x == ',' then ' ' else x) $ drop 12 x
                    | x <- lines src, "tested-with:" `isPrefixOf` x]
    where
        f x | Just rest <- stripPrefix "GHC==" x = rest
            | otherwise = error $ "Invalid tested-with, " ++ x


checkReadme :: IO ()
checkReadme = do
    project <- takeBaseName . fromMaybe (error "Couldn't find cabal file") <$> findCabal
    let want =
            "[![Hackage version](https://img.shields.io/hackage/v/" ++ project ++ ".svg?style=flat)]" ++
            "(http://hackage.haskell.org/package/" ++ project ++ ") " ++
            "[![Build Status](http://img.shields.io/travis/ndmitchell/" ++ project ++ ".svg?style=flat)]" ++
            "(https://travis-ci.org/ndmitchell/" ++ project ++ ")"
    src <- readFile "README.md"
    let line1 = head $ lines src ++ [""]
    when (not $ want `isSuffixOf` line1) $
        error $ "Expected first line of README.md to end with:\n" ++ want ++ "\nBut got:\n" ++ line1


checkCabalFile :: IO ()
checkCabalFile = do
    project <- takeBaseName . fromMaybe (error "Couldn't find cabal file") <$> findCabal 
    src <- fmap lines readCabal
    test <- testedWith
    let grab tag = [stripStart $ drop (length tag + 1) x | x <- relines src, (tag ++ ":") `isPrefixOf` x]
    license <- readFile' $ concat $ grab "license-file"
    let bad =
            ["Incorrect declaration style: " ++ x
                | (x,':':_) <- map (break (== ':') . stripStart) src
                , not $ any isSpace $ strip x, not $ "http" `isSuffixOf` x || "https" `isSuffixOf` x
                , not $ all (\x -> isLower x || x == '-') x] ++
            ["2014 is not in the copyright year" | not $ "2014" `isInfixOf` concat (grab "copyright")] ++
            ["copyright string is not at the start of the license" | not $ concat (grab "copyright") `isInfixOf` concat (take 1 $ lines license)] ++
            ["No correct source-repository link"
                | let want = "source-repository head type: git location: https://github.com/ndmitchell/" ++ project ++ ".git"
                , not $ want `isInfixOf` unwords (words $ unlines src)] ++
            ["No bug-reports link" | grab "bug-reports" /= ["https://github.com/ndmitchell/" ++ project ++ "/issues"]] ++
            ["Incorrect license " | grab "license" `notElem` [["BSD3"],["MIT"]]] ++
            ["Invalid tested-with: " ++ show test | length test < 1 || not (null $ test \\ defAllow) || test /= reverse (sort test) || not (test `isPrefixOf` reverse defAllow)] ++
            ["Bad stabilty, should be missing" | grab "stability" /= []] ++
            ["Missing CHANGES.txt in extra-source-files" | "CHANGES.txt" `notElem` concatMap words (grab "extra-source-files")]
    unless (null bad) $ error $ unlines bad

relines :: [String] -> [String]
relines (x:xs) | ":" `isSuffixOf` x = unwords (x:a) : relines b
    where (a,b) = break (\x -> stripStart x == x) xs
relines (x:xs) = x : relines xs
relines [] = []

readCabal :: IO String
readCabal = do
    file <- findCabal
    case file of
        Nothing -> return []
        Just file -> readFile' file


findCabal :: IO (Maybe FilePath)
findCabal = do
    x <- getDirectoryContents "."
    return $ listToMaybe $ filter ((==) ".cabal" . takeExtension) x
