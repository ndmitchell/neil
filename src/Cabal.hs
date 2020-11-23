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
ghcReleases = ["8.0","8.2","8.4","8.6","8.8","8.10"]

-- | The next version of GHC that has not yet been released
--   (might test it in Travis, but won't be in the tested-with)
ghcNext = Nothing -- Just "8.10" -- set this to "8.10" once rc1 is out

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
    let extend x = fromMaybe x $ lookup (x++".") [(dropWhileEnd (/= '.') v, v) | v <- ghcReleases ++ maybeToList ghcNext]
    -- Add a nub since you might write an entry twice, once with expected_failures
    let tested = nubSort [extend $ fst $ word1 num | Just (pre, num) <- map (stripInfix "GHCVER=") src, '#' `notElem` pre]
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
    ,"cmd","data CmdOption" -- reexported from shake through rattle, so lacks docs
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
withSDist :: Bool -> String -> IO a -> IO a
withSDist no_warnings prefix run = withTempDir $ \tdir -> do
    unless no_warnings $
        system_ "git diff --stat --exit-code"
    local <- map normalise . lines <$> systemOutput_ "git ls-files . --others"
    system_ $ "cabal " ++ prefix ++ "configure --builddir=" ++ tdir
    system_ $ "cabal " ++ prefix ++ "sdist --builddir=" ++ tdir
    files <- listFilesRecursive tdir
    let tarball = head [x | x <- files, ".tar.gz" `isSuffixOf` x]
    withCurrentDirectory tdir $ system_ $ "tar -xf " ++ drop (length tdir + 1) tarball
    lst <- listFilesRecursive tdir
    let bad = local `intersect` map (normalise . drop (length tdir + length tarball - 5)) lst
    when (bad /= []) $
        error $ unlines $ "The following files are not checked in, but are in the dist" : bad
    let binary = [".png",".gz",".bat",".zip",".gif",""]
    bad <- flip filterM lst $ \file ->
        return (takeExtension file `notElem` binary) &&^
        fmap badWhite (readFileBinary' file)
    when (bad /= []) $ do
        error $ unlines $ "The following files have \\r characters, trailing whitespace or excess newlines." : bad
    -- Must canonicalize due to Cabal bug, see https://github.com/haskell/cabal/issues/7170
    path <- canonicalizePath $ tdir </> dropExtension (dropExtension $ takeFileName tarball)
    withCurrentDirectory path run

badWhite :: String -> Bool
badWhite x = '\r' `elem` x || " \n" `isInfixOf` x || "\n\n" `isSuffixOf` x


run :: Arguments -> Maybe (IO ())
run Test{..} = Just $ do
    cabalCheck

    hasLibrary <- maybeM (return True) (fmap ("library" `isInfixOf`) . readFile) findCabal
    hasExecutable <- maybeM (return True) (fmap ("executable" `isInfixOf`) . readFile) findCabal
    ghcVer <- fst . line1 <$> systemOutput_ "ghc --numeric-version"

    let prefix = if cabal2 then "new-" else "v1-"
    withSDist no_warnings prefix $ do
        Just (takeBaseName -> project) <- findCabal
        system_ $ "cabal " ++ (if cabal2 then "new-build" else "v1-install") ++ " --only-dependencies --enable-tests"
        let ghcOptions = "-rtsopts" : "-fwarn-tabs" : ghcWarnings ++
                         ["-Werror" | not no_warnings]
        if cabal2 then do
            writeFile "cabal.project.local" $ unlines
                ["library-profiling: False"
                ,"tests: True"
                ,"package " ++ project
                ,"  ghc-options: " ++ unwords ghcOptions]
         else
            system_ $ unwords $
                ("cabal v1-configure --enable-tests --disable-library-profiling") :
                map ("--ghc-option=" ++) ghcOptions
        system_ $ "cabal " ++ prefix ++ "build"
        when hasLibrary $ do
            if cabal2 then
                system_ $ "cabal new-haddock --haddock-hoogle"
            else
                system_ $ "cabal v1-haddock --hoogle"
            when (ghcVer `elem` takeEnd 2 ghcReleases) $ do
                -- earlier Haddock's forget to document class members in the --hoogle
                checkHoogle
        when (hasExecutable && install) $
            if cabal2 then
                system_ $ "cabal new-install --install-method=copy --overwrite-policy=always"
            else do
                system_ $ "cabal " ++ prefix ++ "copy"
                system_ $ "cabal " ++ prefix ++ "register"
        if cabal2 then
            -- Try and make imported packages available while testing
            system_ "cabal new-exec cabal new-test"
        else
            system_ $ "cabal " ++ prefix ++ "test --show-details=streaming"

run Check{..} = Just $ withCurrentDirectory path cabalCheck

run Sdist = Just $ do
    cabalCheck
    tested <- testedWith
    withSDist False "v1-" $ do
        system_ "cabal v1-clean"
        system_ "cabal v1-install --only-dependencies"
        system_ $ "cabal v1-configure --ghc-option=-fwarn-unused-imports --disable-library-profiling " ++
                  "--ghc-option=-Werror " ++
                  -- Ignore warnings in Cabal generated files :(
                  "--ghc-option=-fno-warn-warnings-deprecations --ghc-option=-fno-warn-unsupported-calling-conventions"
        system_ "cabal v1-build"
        system_ "cabal v1-haddock"
    system_ "cabal v1-sdist"
    putStrLn $ "Ready to release! (remember to neil tag after uploading)"

run Docs{..} = Just $ do
    src <- readCabal
    let ver = extractCabal "version" src
    let name = extractCabal "name" src
    system_ $ "cabal v1-haddock --hoogle --html --hyperlink-source " ++
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
            ,"[![Linux build status](https://img.shields.io/travis/" ++ travis ++ "/master.svg?label=Linux%20build)]" ++
             "(https://travis-ci.org/" ++ travis ++ ")"
            ,"[![Windows build status](https://img.shields.io/appveyor/ci/" ++ appveyor ++ "/master.svg?label=Windows%20build)]" ++
             "(https://ci.appveyor.com/project/" ++ appveyor ++ ")"
            ,"[![Build status](https://img.shields.io/travis/" ++ travis ++ "/master.svg?label=Build)]" ++
             "(https://travis-ci.org/" ++ travis ++ ")"
            ]
    let line1 = head $ src ++ [""]
    let bangs = length $ filter (== '!') line1
    let foundBadges = filter (`isInfixOf` line1) badges
    let found = length foundBadges
    when (found < 2) $
        error $ "Expected first line of README.md to end with at least 2 badges, got:\n" ++ unlines foundBadges ++ "Expected from:\n" ++ unlines badges
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
            ["Invalid tested-with: " ++ show test ++ "\nShould be prefix of " ++ show (reverse ghcReleases) | not $ validTests test] ++
            ["Bad stabilty, should be missing" | grab "stability" /= []] ++
            ["Missing CHANGES.txt in extra-doc-files" | ["CHANGES.txt","CHANGELOG.md","changelog.md"] `disjoint` concatMap words (grab "extra-doc-files")] ++
            ["Missing README.md in extra-doc-files" | "README.md" `notElem` concatMap words (grab "extra-doc-files")] ++
            ["Not all flag's have manual attributes" | let flag = length $ filter ("flag " `isPrefixOf`) src, let manual = length $ filter ("manual:" `isPrefixOf`) $ map trimStart src, flag /= manual]
    unless (null bad) $ error $ unlines bad

validTests :: [String] -> Bool
validTests xs = length xs >= 1 && xs `isPrefixOf` reverse ghcReleases

projectName x = owner ("https://github.com/" ++ ownerGithub x ++ "/") x
ownerGithub = owner " https://github.com/" -- leading space ensures other <https:// links don't pollute the owner
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
    return $ find ((==) ".cabal" . takeExtension) x
