{-# LANGUAGE RecordWildCards #-}

module Git(run) where

import Control.Monad
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Util
import Arguments


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
            withDirectory x $ act x


---------------------------------------------------------------------
-- COMMANDS

run :: Arguments -> Maybe (IO ())

run Whatsnew{..} = Just $ forEachRepo $ \name -> do
    changes <- fmap (length . filter (not . null) . lines) $ cmdOut $ "git status --porcelain --untracked-files=no"
    adds <- if not look_for_adds then return 0 else fmap (subtract changes . length . filter (not . null) . lines) $ cmdOut "git status --porcelain"
    unless local $ cmd "git fetch --quiet"
    local <- fmap read $ cmdOut $ "git rev-list origin/master..master --count"
    remote <- fmap read $ cmdOut $ "git rev-list master..origin/master --count"
    let items = [changes,adds,local,remote]
    let names = [("local change","s"),("addable",""),("local patch","es"),("remote patch","es")]
    let res = [show n ++ " " ++ s ++ (if n == 1 then "" else ss) | (n,(s,ss)) <- zip items names, n /= 0]
    unless (null res) $ putStrLn $ name ++ ": " ++ intercalate ", " res

run _ = Nothing
