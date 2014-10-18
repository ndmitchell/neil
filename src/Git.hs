{-# LANGUAGE RecordWildCards #-}

module Git(run) where

import Control.Monad
import Data.List.Extra
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.Process.Extra
import Arguments
import Cabal(readCabal)


---------------------------------------------------------------------
-- UTILITIES

-- Find all the repos. Look for .git (I am a repo) and */.git (I have child repos)
-- If you don't find any, look up.
findRepos :: IO [FilePath]
findRepos = do
    putStr "Looking for repos... "
    dirs <- getDirectoryContents "."
    res <- filterM (\x -> doesDirectoryExist $ x </> ".git") dirs
    putStrLn $ show (length res) ++ " found"
    return $ sort res


-- Run over each repo
forEachRepo :: (String -> IO ()) -> IO ()
forEachRepo act = do
    res <- findRepos
    when (null res) $ do
        putStrLn $ "Error: No repos found relative to here"
        exitFailure
    forM_ res $ \x ->
        if x == "." then do
            cdir <- getCurrentDirectory
            act $ takeFileName cdir
        else
            withCurrentDirectory x $ act x


---------------------------------------------------------------------
-- COMMANDS

run :: Arguments -> Maybe (IO ())

run Whatsnew{..} = Just $ forEachRepo $ \name -> do
    changes <- fmap (length . filter (not . null) . lines) $ systemOutput_ $ "git status --porcelain --untracked-files=no"
    adds <- if not look_for_adds then return 0 else fmap (subtract changes . length . filter (not . null) . lines) $ systemOutput_ "git status --porcelain"
    unless local $ system_ "git fetch --quiet"
    local <- fmap read $ systemOutput_ $ "git rev-list origin/master..master --count"
    remote <- fmap read $ systemOutput_ $ "git rev-list master..origin/master --count"
    let items = [changes,adds,local,remote]
    let names = [("local change","s"),("addable",""),("local patch","es"),("remote patch","es")]
    let res = [show n ++ " " ++ s ++ (if n == 1 then "" else ss) | (n,(s,ss)) <- zip items names, n /= 0]
    unless (null res) $ putStrLn $ name ++ ": " ++ intercalate ", " res

run Tag = Just $ do
    src <- readCabal
    let [ver] = [trim $ drop 8 x | x <- lines src, "version:" `isPrefixOf` x]
    putStrLn $ "Confirm to tag the release with version " ++ ver ++ "? Type 'yes':"
    "yes" <- getLine
    system_ "git push"
    system_ $ "git tag v" ++ ver
    system_ "git push --tags"

run _ = Nothing
