
module Darcs(push,pull,send,whatsnew,apply) where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Util
import Control.Exception as E
import Control.Concurrent
import Data.Maybe


---------------------------------------------------------------------
-- UTILITIES

-- Find all the repos. Look down searching for a _darcs, if you find any, return them.
-- If you don't find any, look up.
findRepos :: FilePath -> IO [FilePath]
findRepos x = do
    putStr "Looking for repos... "
    x <- canonicalizePath x
    xs <- searchDown x
    res <- if null xs then searchUp x else return xs
    putStrLn $ show (length res) ++ " found"
    return res
    where
        searchDown dir = do
            b <- doesDirectoryExist $ dir </> "_darcs"
            if b then return [dir] else do
                xs <- getDirectoryContents dir
                xs <- return [dir </> x | x <- xs, not $ all (== '.') x]
                fmap concat $ forM xs $ \x -> do
                    b <- doesDirectoryExist x
                    if b then searchDown x else return []

        searchUp x = return []


-- Run over each repo
forEachRepo :: Bool -> FilePath -> (FilePath -> IO (Maybe String)) -> IO ()
forEachRepo deleteLocks repo act = do
    res <- fmap sort $ findRepos repo
    when (null res) $ do
        putStrLn $ "Error: No repos found relative to " ++ show repo
        exitFailure

    ln <- liner
    finished <- newEmptyMVar
    todo <- newMVar res
    let done x msg = modifyMVar_ todo $ \old -> do
        let new = delete x old
        when (isJust msg) $ ln $ takeFileName x ++ ": " ++ fromJust msg ++ "\n"
        if null new then do
            ln "Finished\n"
            putMVar finished ()
         else
            ln $ "Waiting for " ++ show (length new) ++ " (" ++ unwords (map takeFileName $ take 4 new) ++ "...)"
        return new

    done "" Nothing
    forM_ res $ \repo -> forkIO $ do
        let lockFile = repo </> "_darcs/lock"
        b <- doesFileExist lockFile
        ans <- if b && not deleteLocks then
            return $ Just "skipping because it has a lock outstanding"
         else do
            when (b && deleteLocks) $ removeFile lockFile
            act repo
        done repo ans

    takeMVar finished


realLines :: String -> Int
realLines str = length [() | x:_ <- lines str, not $ isSpace x]

---------------------------------------------------------------------
-- RAW COMMANDS

darcsWhatsnew :: FilePath -> IO (Maybe Int)
darcsWhatsnew repo = do
    (code,out,err) <- cmdCodeOutErr $ "darcs whatsnew --summary --repo=" ++ repo
    return $ case code of
        ExitFailure 1 -> Just 0
        ExitSuccess -> Just $ length $ lines out
        _ -> Nothing


darcsPull :: FilePath -> Bool -> IO (Maybe Int)
darcsPull repo dryrun = do
    (code,out,err) <- cmdCodeOutErr $ "darcs pull --all --repo=" ++ repo ++ (if dryrun then " --dry-run" else "")
    return $ case code of
        ExitSuccess -> Just $ realLines out - 2
        _ -> Nothing


darcsSend :: FilePath -> Maybe FilePath -> IO (Maybe Int)
darcsSend repo outfile = do
    (code,out,err) <- cmdCodeOutErr $ "darcs send --all --repo=" ++ repo ++ " " ++
        maybe "--dry-run" (" --output=" ++) outfile
    return $ case code of
        ExitSuccess
            | "No recorded local changes to send!" `elem` lines out -> Just 0
            | otherwise -> Just 1
        _ -> Nothing


---------------------------------------------------------------------
-- COMMANDS

whatsnew :: FilePath -> Bool -> IO ()
whatsnew repo locks = forEachRepo locks repo $ \x ->
    (do
        changes <- darcsWhatsnew x
        local <- darcsSend x Nothing
        remote <- darcsPull x True
        return $ if changes == Just 0 && local == Just 0 && remote == Just 0 then Nothing else
            Just $ intercalate ", "
                [ maybe "?" show n ++ " " ++ s ++ (if n == Just 1 then "" else ss)
                | (n,s,ss) <- [(changes,"local change","s"),(local,"local patch","es"),(remote,"remote patch","es")]
                , n /= Just 0]
    ) `E.onException` (do
        let lockFile = x </> "_darcs/lock"
        b <- doesFileExist lockFile
        when b $ removeFile lockFile
    )


pull :: FilePath -> Bool -> IO ()
pull repo locks = forEachRepo locks repo $ \x -> do
    n <- darcsPull x False
    return $ case n of
        Nothing -> Just "Failed"
        Just 0 -> Nothing
        Just n -> Just $ "Pulled " ++ show n ++ " patch" ++ (if n == 1 then "" else "es")


push :: FilePath -> IO ()
push repo = do
    mvar <- newMVar []
    forEachRepo False repo $ \x -> do
        n <- darcsSend x Nothing
        case n of
            Nothing -> return $ Just "failed to determine if there are patches"
            Just 0 -> return Nothing
            Just n -> do
                modifyMVar_ mvar (return . (:) x)
                return $ Just $ show n ++ " patch" ++ (if n == 1 then "" else "es") ++ " to push"

    res <- readMVar mvar
    forM_ res $ \x -> do
        putStrLn $ "Trying to push from " ++ x
        src <- readFile' $ x </> "_darcs" </> "prefs" </> "repos"
        case pick $ lines src of
            Nothing -> error "Failed: No non-http repos in the prefs/repos file"
            Just r -> do
                cmd $ "darcs push --no-set-default \"" ++ r ++ "\" --repo=" ++ x
    where
        -- pick the ssh address
        pick :: [String] -> Maybe String
        pick xs = listToMaybe $ ssh ++ [b ++ drop (length a) h | h <- http, (a,b) <- mapping, a `isPrefixOf` h]
            where (http,ssh) = partition ("http://" `isPrefixOf`) xs


        mapping = [("http://www.cs.york.ac.uk/fp/darcs/","ndm@community.haskell.org:/home/ndm/darcs/")
                  ,("http://community.haskell.org/~ndm/darcs/","ndm@community.haskell.org:/home/ndm/darcs/")
                  ,("http://code.haskell.org/","ndm@code.haskell.org:/srv/code/")
                  ]


send :: FilePath -> FilePath -> IO ()
send repo patch = do
    mvar <- newMVar []
    forEachRepo False repo $ \x -> do
        n <- darcsSend x Nothing
        return $ Just $ "I want to send " ++ show n


apply = undefined
