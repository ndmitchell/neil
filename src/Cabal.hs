{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns #-}

module Cabal(run, readCabal) where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Functor
import System.Directory.Extra
import System.IO.Extra
import System.FilePath
import System.Process.Extra
import Arguments

defAllow = ["7.0.4","7.2.2","7.4.2","7.6.3","7.8.3"]


---------------------------------------------------------------------
-- COMMANDS


-- | Check the .cabal file is well formed
cabalCheck :: IO ()
cabalCheck = do
    -- a lot of the warnings aren't real problems, so whitelist some
    (_, res) <- systemOutput "cabal check"
    let allowed = ["No errors or warnings could be found in the package."
                  ,"These warnings may cause trouble when distributing the package:"
                  ,"* 'ghc-options: -main-is' is not portable."
                  ,""]
    let bad = lines res \\ allowed
    when (bad /= []) $ error $ unlines $ "Cabal check gave bad warnings:" : map show bad

    checkCabalFile
    checkReadme
    let require = ":set -fwarn-unused-binds -fwarn-unused-imports"
    src <- readFile' ".ghci"
    when (require `notElem` lines src) $
        error $ "The .ghci file does not contain " ++ require

-- | Run some commands in a temporary directory with the unpacked cabal
withSDist :: IO a -> IO a
withSDist run = withTempDir $ \tdir -> do
    system_ $ "cabal configure --builddir=" ++ tdir
    system_ $ "cabal sdist --builddir=" ++ tdir
    files <- getDirectoryContents tdir
    let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withCurrentDirectory tdir $ system_ $ "tar -xf " ++ tarball
    lst <- listFilesRecursive tdir
    let binary = [".png",".gz",".bat",".zip",".gif",""]
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
        system_ "cabal install --only-dependencies"
        system_ $ "cabal configure --enable-tests --disable-library-profiling " ++
              "--ghc-option=-fwarn-unused-binds --ghc-option=-fwarn-unused-imports " ++
              "--ghc-option=-fwarn-tabs " ++
              "--ghc-option=-Werror"
        system_ "cabal build"
        system_ "cabal test --show-details=always"
        when install $
            system_ "cabal install --force-reinstalls"

run Check = Just cabalCheck

run Sdist{..} = Just $ do
    cabalCheck
    tested <- testedWith
    withSDist $ do
        forM_ (sort tested) $ \x -> do -- deliberately start with the oldest first
            putStrLn $ "Building with " ++ x
            system_ "cabal clean"
            system_ $ "cabal install --only-dependencies " ++
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            system_ $ "cabal configure --ghc-option=-fwarn-unused-imports --disable-library-profiling " ++
                  "--ghc-option=-Werror --ghc-option=-fno-warn-warnings-deprecations " ++ -- CABAL BUG WORKAROUND :(
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            system_ "cabal build"
            system_ "cabal haddock --executables"
    system_ "cabal sdist"
    putStrLn $ "Ready to release! (remember to neil tag after uploading)"

run Docs{..} = Just $ do
    src <- readCabal
    let ver = extractCabal "version" src
    let name = extractCabal "name" src
    system_ $ "cabal haddock --hoogle --hyperlink-source " ++
          "--contents-location=/package/" ++ name
    withTempDir $ \dir -> do
        system_ $ "cp -R dist/doc/html/" ++ name ++ " \"" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs\""
        files <- listFilesRecursive dir
        forM_ files $ \file -> when (takeExtension file == ".html") $ do
            src <- readFileBinary' $ dir </> file
            src <- return $ filter (/= '\r') src -- filter out \r, due to CPP bugs
            src <- return $ fixFileLinks $ fixHashT src
            writeFileBinary (dir </> file) src
        system_ $ "tar cvz -C " ++ dir ++ " --format=ustar -f " ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz " ++ name ++ "-" ++ ver ++ "-docs"
        system_ $ "curl -X PUT -H \"Content-Type: application/x-tar\" " ++
              "-H \"Content-Encoding: gzip\" " ++
              "-u " ++ username ++ " " ++
              "--data-binary \"@" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz\" " ++
              host ++ "/package/" ++ name ++ "-" ++ ver ++ "/docs"

run _ = Nothing


fixHashT :: String -> String
fixHashT (stripPrefix ".html#t:" -> Just (x:xs)) | not $ isUpper x = ".html#v:" ++ fixHashT (x:xs)
fixHashT (x:xs) = x : fixHashT xs
fixHashT [] = []

fixFileLinks :: String -> String
fixFileLinks (stripPrefix "<a href=\"file://" -> Just xs)
    | (a,'\"':b) <- break (== '\"') xs
    , modu <- takeFileName a
    , pkg <- dropEnd 1 $ dropWhileEnd (/= '-') $ takeFileName $ dropHTML $ takeDirectory a
    = "<a href=\"/package/" ++ pkg ++ "/docs/" ++ modu ++ "\"" ++ fixFileLinks b
    where dropHTML x = if takeFileName x == "html" then takeDirectory x else x
fixFileLinks xs@(stripPrefix "<a href=\"file://" -> Just _) = error $ "Unable to remove file link, " ++ take 200 xs
fixFileLinks (x:xs) = x : fixFileLinks xs
fixFileLinks [] = []


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
            "[![Build Status](http://img.shields.io/travis/" ++ qualify project ++ ".svg?style=flat)]" ++
            "(https://travis-ci.org/" ++ qualify project ++ ")"
    src <- readFile "README.md"
    let line1 = head $ lines src ++ [""]
    when (not $ want `isSuffixOf` line1) $
        error $ "Expected first line of README.md to end with:\n" ++ want ++ "\nBut got:\n" ++ line1


checkCabalFile :: IO ()
checkCabalFile = do
    project <- takeBaseName . fromMaybe (error "Couldn't find cabal file") <$> findCabal 
    src <- fmap lines readCabal
    test <- testedWith
    let grab tag = [trimStart $ drop (length tag + 1) x | x <- relines src, (tag ++ ":") `isPrefixOf` x]
    license <- readFile' $ concat $ grab "license-file"
    let bad =
            ["Incorrect declaration style: " ++ x
                | (x,':':_) <- map (break (== ':') . trimStart) src
                , not $ any isSpace $ trim x, not $ "http" `isSuffixOf` x || "https" `isSuffixOf` x
                , not $ all (\x -> isLower x || x == '-') x] ++
            ["2014 is not in the copyright year" | not $ "2014" `isInfixOf` concat (grab "copyright")] ++
            ["copyright string is not at the start of the license" | not $ concat (grab "copyright") `isInfixOf` concat (take 1 $ lines license)] ++
            ["No correct source-repository link"
                | let want = "source-repository head type: git location: https://github.com/" ++ qualify project ++ ".git"
                , not $ want `isInfixOf` unwords (words $ unlines src)] ++
            ["No bug-reports link" | grab "bug-reports" /= ["https://github.com/" ++ qualify project ++ "/issues"]] ++
            ["Incorrect license " | grab "license" `notElem` [["BSD3"],["MIT"]]] ++
            ["Invalid tested-with: " ++ show test | length test < 1 || not (null $ test \\ defAllow) || test /= reverse (sort test) || not (test `isPrefixOf` reverse defAllow)] ++
            ["Bad stabilty, should be missing" | grab "stability" /= []] ++
            ["Missing CHANGES.txt in extra-source-files" | ["CHANGES.txt","changelog.md"] `disjoint` concatMap words (grab "extra-source-files")]
    unless (null bad) $ error $ unlines bad

qualify :: String -> String
qualify "filepath" = "haskell/filepath"
qualify x = "ndmitchell/" ++ x

relines :: [String] -> [String]
relines (x:xs) | ":" `isSuffixOf` x = unwords (x:a) : relines b
    where (a,b) = break (\x -> trimStart x == x) xs
relines (x:xs) = x : relines xs
relines [] = []

readCabal :: IO String
readCabal = do
    file <- findCabal
    case file of
        Nothing -> return []
        Just file -> readFile' file

extractCabal :: String -> String -> String
extractCabal find = f . words . replace ":" " : "
    where
        f (name:":":val:_) | lower find == lower name = val
        f (x:xs) = f xs
        f [] = error "Failed to find the Cabal key " ++ find


findCabal :: IO (Maybe FilePath)
findCabal = do
    x <- getDirectoryContents "."
    return $ listToMaybe $ filter ((==) ".cabal" . takeExtension) x
