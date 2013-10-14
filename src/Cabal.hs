{-# LANGUAGE RecordWildCards #-}

module Cabal(run) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Functor
import System.Directory
import System.FilePath
import Util
import Arguments

-- Policy: currently all must build flawlessly on 6.12.3, 7.0.2 and 7.2.1, and at least build on 6.10.4
defOfficial = ["6.12.3","7.0.4","7.2.2","7.4.1","7.6.1"]
defPartial = ["6.10.4"]


---------------------------------------------------------------------
-- COMMANDS


-- | Check the .cabal file is well formed
cabalCheck :: IO ()
cabalCheck = do
    res <- cmdCode "cabal check"
    checkCabalFile

-- | Run some commands in a temporary directory with the unpacked cabal
withSDist :: IO a -> IO a
withSDist run = withTempDirectory $ \tdir -> do
    cmd $ "cabal configure --builddir=" ++ tdir
    cmd $ "cabal sdist --builddir=" ++ tdir
    files <- getDirectoryContents tdir
    let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withDirectory tdir $ cmd $ "tar -xf " ++ tarball
    withDirectory (tdir </> dropExtension (dropExtension $ takeFileName tarball)) run


run :: Arguments -> Maybe (IO ())
run Test = Just $ do
    cabalCheck
    withSDist $ do
        cmd "cabal install --only-dependencies"
        cmd $ "cabal configure --enable-tests --disable-library-profiling " ++
              {- "--ghc-option=-Werror -} "--ghc-option=-fno-warn-warnings-deprecations" -- CABAL BUG WORKAROUND :(
        cmd "cabal build"
        cmd "cabal test --show-details=always"

run Check = Just cabalCheck

run Sdist{..} = Just $ do
    cabalCheck
    tested <- testedWith
    tested <- return $ if null tested then [""] else tested
    withSDist $ do
        let a +| b = if null a then b else a
        forM_ (official +| defOfficial) $ \x -> do
            putStrLn $ "Building with " ++ x
            cmd "cabal clean"
            cmd $ "cabal install --only-dependencies " ++
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            cmd $ "cabal configure --ghc-option=-fwarn-unused-imports --disable-library-profiling " ++
                  (if ignore_warnings then "" else "--ghc-option=-Werror ") ++
                  "--ghc-option=-fno-warn-warnings-deprecations " ++ -- CABAL BUG WORKAROUND :(
                  "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                  "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe " ++
                  "--flags=testprog"
            cmd "cabal build"
            cmd "cabal haddock --executables"
        unless ignore_partial $ do
            forM_ (partial +| defPartial) $ \x -> do
                putStrLn $ "Building with " ++ x
                cmd "cabal clean"
                cmd $ "cabal install --only-dependencies " ++
                      "--with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-haddock=c:\\ghc\\ghc-" ++ x ++ "\\bin\\haddock.exe " ++
                      "--with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe"
                cmd $ "cabal configure --disable-library-profiling --with-compiler=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc.exe --with-hc-pkg=c:\\ghc\\ghc-" ++ x ++ "\\bin\\ghc-pkg.exe --flags=testprog"
                cmd "cabal build"
    cmd "cabal sdist"
    putStrLn $ "Ready to release!"

run _ = Nothing



testedWith :: IO [String]
testedWith = do
    src <- readCabal
    return $ concat [ map f $ words $ map (\x -> if x == ',' then ' ' else x) $ drop 12 x
                    | x <- lines src, "tested-with:" `isPrefixOf` x]
    where
        f x = map toLower a ++ "-" ++ drop 2 b
            where (a,b) = break (== '=') x


checkCabalFile :: IO ()
checkCabalFile = do
    project <- takeBaseName . fromMaybe (error "Couldn't find cabal file") <$> findCabal 
    src <- fmap lines readCabal
    let grab tag = [trimLeft $ drop (length tag + 1) x | x <- src, (tag ++ ":") `isPrefixOf` x]
    license <- readFile' $ concat $ grab "license-file"
    let bad =
            ["Incorrect declaration style: " ++ x
                | (x,':':_) <- map (break (== ':') . trimLeft) src
                , not $ any isSpace $ trim x, not $ "http" `isSuffixOf` x, not $ all (\x -> isLower x || x == '-') x] ++
            ["2013 is not in the copyright year" | not $ "2013" `isInfixOf` concat (grab "copyright")] ++
            ["copyright string is not at the start of the license" | not $ concat (grab "copyright") `isInfixOf` concat (take 1 $ lines license)] ++
            ["No correct source-repository link"
                | let want = "source-repository head type: git location: https://github.com/ndmitchell/" ++ project ++ ".git"
                , not $ want `isInfixOf` unwords (words $ unlines src)] ++
            ["Incorrect license " | grab "license" /= ["BSD3"]]
    unless (null bad) $ error $ unlines bad

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
