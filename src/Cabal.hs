{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns, TupleSections #-}

module Cabal(run, readCabal, extractCabal, relines) where

import Control.Monad.Extra
import Control.Exception.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Functor
import Data.Tuple.Extra
import System.Directory.Extra
import System.IO.Extra
import System.FilePath
import System.Process.Extra
import Arguments
import Prelude

-- | GHC releases I test with
ghcReleases = ["7.4.2","7.6.3","7.8.4","7.10.3","8.0.2","8.2.2","8.4.4","8.6.3"]

-- | The next version of GHC that has not yet been released
--   (might test it in Travis, but won't be in the tested-with)
ghcNext = Nothing -- set this to "8.8.1" once rc1 is out

ghcWarnings = words "-fwarn-unused-binds -fwarn-unused-imports -fwarn-orphans"


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
                  ,"* Packages relying on Cabal 1.12 or later should specify a version range of"
                  ,"the form 'cabal-version: x.y'. Use 'cabal-version: 1.18'."
                  ,"Warning: These warnings may cause trouble when distributing the package:"
                  ,"Warning: 'ghc-options: -main-is' is not portable."
                  ,"Warning: Packages relying on Cabal 1.12 or later should specify a specific"
                  ,"version of the Cabal spec of the form 'cabal-version: x.y'. Use"
                  ,"'cabal-version: 1.18'."
                  ]
    let bad = filter (not . null) (lines res) \\ allowed
    when (bad /= []) $ error $ unlines $ "Cabal check gave bad warnings:" : map show bad

    checkCabalFile
    checkReadme
    checkChangelog
    checkGhci
    checkTravis
    checkAppveyor
    checkPullRequestTemplate


checkGhci :: IO ()
checkGhci = do
    src <- words <$> readFile' ".ghci"
    unless ("-W" `elem` src || all (`elem` src) ghcWarnings) $
        error $ "The .ghci file does not enough of " ++ unwords ("-W":ghcWarnings)


checkTravis :: IO ()
checkTravis = do
    src <- lines <$> readFile' ".travis.yml"

    let script = "- curl -sSL https://raw.github.com/<github_user>/neil/<commit>/travis.sh | sh -s <github_user> <commit>"
    let isScript x = case words x of
            ["-","curl","-sSL","https://raw.github.com/ndmitchell/neil/master/travis.sh","|","sh"] -> True
            ["-","curl","-sSL",url,"|","sh","-s",user,commit] -> url == ("https://raw.github.com/" ++ user ++ "/neil/" ++ commit ++ "/travis.sh")
            _ -> False

    when (length (filter isScript src) /= 1) $
        fail $ "Expect to see exactly one script but not, please add: " ++ script

    claimed <- (\xs -> sort $ "head" : maybeToList ghcNext ++ xs) <$> testedWith
    -- Add a nub since you might write an entry twice, once with expected_failures
    let tested = nubSort [fst $ word1 num | Just (_, num) <- map (stripInfix "GHCVER=") src]
    when (claimed /= tested) $
        fail $ "Difference between the .cabal and the travis, " ++ show claimed ++ " vs " ++ show tested

checkAppveyor :: IO ()
checkAppveyor = do
    let required =
            ["build: off"
            ,"cache: \"c:\\\\sr -> appveyor.yml\""
            ,"test_script:"
            ,"- ps: Invoke-Expression (Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/neil/master/appveyor.ps1')"]
    whenM (doesFileExist "appveyor.yml") $ do
        src <- map (trimEnd . takeWhile (/= '#')) . lines <$> readFile' "appveyor.yml"
        let missing = required \\ src
        when (missing /= []) $
            fail $ unlines $ "Missing some important lines in appveyor.yml" : missing


undocumented :: [String]
undocumented =
    ["safe" -- The lack of documentation is deliberate, since the pattern is more important
    ,"nsis" -- Issue to fix it at https://github.com/ndmitchell/nsis/issues/5
    ,"debug" -- Issue to fix it at https://github.com/ndmitchell/debug/issues/29
    ]


-- | Check every function exported is also documented
checkHoogle :: IO ()
checkHoogle = whenM (doesDirectoryExist "dist/doc/html") $ do
    xs <- listContents "dist/doc/html"
    -- I don't care that Derive isn't fully documented
    forM_ xs $ \x -> do
        let file = x </> takeFileName x <.> "txt"
        contents <- readFileUTF8' file
        -- look for two lines in a row not separated by comments
        let bad = missingDocs $ wordsBy ("--" `isPrefixOf`) $
                  filter (\x -> not $ any (`isPrefixOf` x) docWhitelist) $
                  filter (not . null) $ map trim $ lines contents
        if bad == [] then
            putStrLn $ "Hoogle check successful for " ++ file
         else do
            putStr $ unlines $ "Bad hoogle:" : bad
            if (takeFileName x `elem` undocumented) then
                putStrLn $ "Lack of complete documentation is known for " ++ file
             else
                error "Found bad Hoogle entries"


docWhitelist :: [String]
docWhitelist =
    ["infix ","infixl ","infixr "
    ,"instance "
    ,"@version "
    ,"(==)","(/=)","fromString " -- not documented in base
    ,"class Hashable ","hashWithSalt ","hash ","unit "
        -- don't seem to end up present when exported through Shake on Travis
    ]

-- | Given a set of definitions, each preceeded by docs, return the bad definitions.
--   Generally that's any non-leading definition, but don't require docs on constructors or selectors.
missingDocs :: [[String]] -> [String]
missingDocs xss = f "" $ concat [(True,x) : map (False,) xs | x:xs <- xss]
    where
        isCtor (x:xs) | isUpper x = True
        isCtor ('[':x:xs) | isUpper x = True -- later Haddock sometimes writes constructors [Foo]
        isCtor _ = False

        isSelector ctor x
            | Just (fields,result) <- unsnoc $ parseType ctor
            , [first,rest] <- parseType x
            , first == result
            , rest `elem` fields
            = True
        isSelector _ _ = False

        f ctor ((doc,x):xs) =
            [x | not $ doc || isCtor x || isSelector ctor x] ++
            f (if isCtor x then x else ctor) xs
        f ctor [] = []

parseType :: String -> [String]
parseType = map trim . splitOn "->" . drop 2 . snd . breakOn "::"


-- | Run some commands in a temporary directory with the unpacked cabal
withSDist :: Bool -> IO a -> IO a
withSDist no_warnings run = withTempDir $ \tdir -> do
    unless no_warnings $
        system_ "git diff --stat --exit-code"
    local <- map normalise . lines <$> systemOutput_ "git ls-files . --others"
    system_ $ "cabal configure --builddir=" ++ tdir
    system_ $ "cabal sdist --builddir=" ++ tdir
    files <- getDirectoryContents tdir
    let tarball = head $ [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withCurrentDirectory tdir $ system_ $ "tar -xf " ++ tarball
    lst <- listFilesRecursive tdir
    let bad = local `intersect` map (normalise . drop (length tdir + length tarball - 5)) lst
    when (bad /= []) $
        error $ unlines $ "The following files are not checked in, but are in the dist" : bad
    let binary = [".png",".gz",".bat",".zip",".gif",""]
    bad <- flip filterM lst $ \file ->
        return (takeExtension file `notElem` binary) &&^
        fmap ('\r' `elem`) (readFileBinary' file)
    when (bad /= []) $ do
        error $ unlines $ "The following files have \\r characters in, Windows newlines?" : bad
    withCurrentDirectory (tdir </> dropExtension (dropExtension $ takeFileName tarball)) run


run :: Arguments -> Maybe (IO ())
run Test{..} = Just $ do
    test <- cabalCheck

    runTest <- maybe (return True) (fmap ("test-suite" `isInfixOf`) . readFile) =<< findCabal
    ghcVer <- fst . line1 <$> systemOutput_ "ghc --numeric-version"

    withSDist no_warnings $ do
        system_ "cabal install --only-dependencies --enable-tests"
        let ghcOptions = "-rtsopts" : "-fwarn-tabs" : ghcWarnings ++
                         (if "7." `isPrefixOf` ghcVer then [] else  words "-Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances") ++
                         ["-Werror" | not no_warnings]
        system_ $ unwords $
            "cabal configure --enable-tests --disable-library-profiling" :
            map ("--ghc-option=" ++) ghcOptions
        system_ "cabal build"
        system_ "cabal haddock --hoogle"
        when (ghcVer `elem` takeEnd 2 ghcReleases) $ do
            -- earlier Haddock's forget to document class members in the --hoogle
            checkHoogle
        when runTest $ system_ "cabal test --show-details=streaming"
        when install $ do
            system_ "cabal copy"
            system_ "cabal register"

run Check{..} = Just $ withCurrentDirectory path cabalCheck

run Sdist = Just $ do
    cabalCheck
    tested <- testedWith
    withSDist False $ do
        system_ "cabal clean"
        system_ "cabal install --only-dependencies"
        system_ $ "cabal configure --ghc-option=-fwarn-unused-imports --disable-library-profiling " ++
                  "--ghc-option=-Werror " ++
                  -- Ignore warnings in Cabal generated files :(
                  "--ghc-option=-fno-warn-warnings-deprecations --ghc-option=-fno-warn-unsupported-calling-conventions"
        system_ "cabal build"
        system_ "cabal haddock"
    system_ "cabal sdist"
    putStrLn $ "Ready to release! (remember to neil tag after uploading)"

run Docs{..} = Just $ do
    src <- readCabal
    let ver = extractCabal "version" src
    let name = extractCabal "name" src
    system_ $ "cabal haddock --hoogle --html --hyperlink-source " ++
          "--contents-location=/package/" ++ name
    withTempDir $ \dir -> do
        system_ $ "cp -R dist/doc/html/" ++ name ++ " \"" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs\""
        files <- listFilesRecursive dir
        forM_ files $ \file -> when (takeExtension file == ".html") $ do
            system_ $ "chmod u+w " ++ (dir </> file)
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
    name <- takeBaseName . fromMaybe (error "Couldn't find cabal file") <$> findCabal
    src <- fmap lines $ readFile "README.md"
    cabal <- fmap lines readCabal
    let project = projectName cabal
    let travis = ownerTravis src ++ "/" ++ project
    let appveyor = ownerAppveyor src ++ "/" ++ name
    let badges =
            ["[![Hackage version](https://img.shields.io/hackage/v/" ++ name ++ ".svg?label=Hackage)]" ++
             "(https://hackage.haskell.org/package/" ++ name ++ ")"
            ,"[![Stackage version](https://www.stackage.org/package/" ++ name ++ "/badge/nightly?label=Stackage)]" ++
             "(https://www.stackage.org/package/" ++ name ++ ")"
            ,"[![Linux Build Status](https://img.shields.io/travis/" ++ travis ++ "/master.svg?label=Linux%20build)]" ++
             "(https://travis-ci.org/" ++ travis ++ ")"
            ,"[![Windows Build Status](https://img.shields.io/appveyor/ci/" ++ appveyor ++ "/master.svg?label=Windows%20build)]" ++
             "(https://ci.appveyor.com/project/" ++ appveyor ++ ")"
            ,"[![Build Status](https://img.shields.io/travis/" ++ travis ++ "/master.svg)]" ++
             "(https://travis-ci.org/" ++ travis ++ ")"
            ]
    let line1 = head $ src ++ [""]
    let bangs = length $ filter (== '!') line1
    let found = length $ filter (`isInfixOf` line1) badges
    when (found < 2) $
        error $ "Expected first line of README.md to end with at least 2 badges, got " ++ show found
    when (found /= bangs) $
        error $ "Unexpected badges, found " ++ show bangs ++ ", but only recognised " ++ show found


checkChangelog :: IO ()
checkChangelog = do
    res <- fromMaybe (error "Couldn't find changelog") <$>
        findM doesFileExist ["CHANGES.txt","changelog.md", "CHANGELOG.md"]
    src <- lines <$> readFile res
    let missingDate = filter (all $ isDigit ||^ (== '.')) $ filter (not . null) src
    when (missingDate /= []) $
        error $ "Expected dates for all releases, but missing for " ++ show missingDate


checkPullRequestTemplate :: IO ()
checkPullRequestTemplate = do
    res <- lines <$> readFile "PULL_REQUEST_TEMPLATE.md"
    let required =
            ["Thanks for the pull request!"
            ,""
            ,"By raising this pull request you confirm you are licensing your contribution under all licenses that apply to this project (see LICENSE) and that you have no patents covering your contribution."
            ,""
            ,"If you care, my PR preferences are at https://github.com/ndmitchell/neil#contributions, but they're all guidelines, and I'm not too fussy - you don't have to read them."
            ]
    when (res /= required) $
        error $ "Expected pull request template in the right form, but not found"


getLatestYear :: IO String
getLatestYear = do
    res <- systemOutput_ "git show -s --format=%ci HEAD"
    let year = takeWhile isDigit res
    when (length year /= 4) $ fail $ "Couldn't get date, " ++ res
    return year


checkCabalFile :: IO ()
checkCabalFile = do
    src <- fmap lines readCabal
    let project = projectName src
    test <- testedWith
    let grab tag = [trimStart $ drop (length tag + 1) x | x <- relines src, (tag ++ ":") `isPrefixOf` x]
    license <- catch_ (readFile' $ concat $ grab "license-file") $ \_ -> return ""
    year <- getLatestYear
    let github = ownerGithub src ++ "/" ++ project
    let bad =
            ["Incorrect declaration style: " ++ x
                | (x,':':_) <- map (break (== ':') . trimStart) src
                , not $ any isSpace $ trim x, not $ "http" `isSuffixOf` x || "https" `isSuffixOf` x
                , not $ all (\x -> isLower x || x == '-') x] ++
            [year ++ " is not in the copyright year" | not $ year `isInfixOf` concat (grab "copyright")] ++
            ["copyright string is not at the start of the license-file" | not $ (concat (grab "copyright") `isInfixOf` concat (take 1 $ lines license)) || grab "license" == ["GPL"]] ++
            ["No correct source-repository link, wanted: " ++ want
                | let want = "source-repository head type: git location: https://github.com/" ++ github ++ ".git"
                , not $ want `isInfixOf` unwords (words $ unlines src)] ++
            ["No bug-reports link" | grab "bug-reports" /= ["https://github.com/" ++ github ++ "/issues"]] ++
            ["Homepage no longer exists" | "~ndm" `isInfixOf` concat (grab "homepage")] ++
            ["Incorrect license" | grab "license" `notElem` [["BSD3"],["MIT"]]] ++
            ["Incorrect default language" | x <- grab "default-language", x /= "Haskell2010"] ++
            ["Invalid tested-with: " ++ show test | not $ validTests test] ++
            ["Bad stabilty, should be missing" | grab "stability" /= []] ++
            ["Missing CHANGES.txt in extra-doc-files" | ["CHANGES.txt","CHANGELOG.md","changelog.md"] `disjoint` concatMap words (grab "extra-doc-files")] ++
            ["Missing README.md in extra-doc-files" | "README.md" `notElem` concatMap words (grab "extra-doc-files")] ++
            ["Not all flag's have manual attributes" | let flag = length $ filter ("flag " `isPrefixOf`) src, let manual = length $ filter ("manual:" `isPrefixOf`) $ map trimStart src, flag /= manual]
    unless (null bad) $ error $ unlines bad

validTests :: [String] -> Bool
validTests xs = length xs >= 1 && xs `isPrefixOf` reverse ghcReleases

projectName x = owner ("https://github.com/" ++ ownerGithub x ++ "/") x
ownerGithub = owner "https://github.com/"
ownerTravis = owner "https://img.shields.io/travis/"
ownerAppveyor = owner "https://img.shields.io/appveyor/ci/"

owner :: String -> [String] -> String
owner fragment src = if x == "" then "ndmitchell" else x
    where x = takeWhile (`notElem` "/#\n") $ drop (length fragment) $ snd $ breakOn fragment $ unlines src


relines :: [String] -> [String]
relines (x:xs) | ":" `isInfixOf` x = unwords (x:a) : relines b
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
