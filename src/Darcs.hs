
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

---------------------------------------------------------------------
-- COMMANDS

whatsnew :: FilePath -> Bool -> IO ()
whatsnew repo locks = forEachRepo locks repo $ \x ->
    (do
        changes <- f ("darcs whatsnew --summary --repo=" ++ x) length
        local <- f ("darcs send --dry-run --all --repo=" ++ x) $ hasnt "No recorded local changes to send!"
        remote <- f ("darcs pull --dry-run --all --repo=" ++ x) $ spaces 2
        return $ if changes == 0 && local == 0 && remote == 0 then Nothing else
            Just $ intercalate ", "
                [ (if n == -1 then "?" else show n) ++ " " ++ s ++ (if n == 1 then "" else ss)
                | (n,s,ss) <- [(changes,"local change","s"),(local,"local patch","es"),(remote,"remote patch","es")]
                , n /= 0]
    ) `E.onException` (do
        let lockFile = x </> "_darcs/lock"
        b <- doesFileExist lockFile
        when b $ removeFile lockFile
    )
    where
        hasnt msg out = if msg `elem` out then 0 else 1
        spaces n out = length [() | x:xs <- out, not $ isSpace x] - n

        f cmd count = do
            (code,out,err) <- cmdCodeOutErr cmd
            case code of
                ExitFailure 1 -> return 0
                ExitSuccess -> return $ count $ lines out
                _ -> return $ negate 1


pull :: FilePath -> Bool -> IO ()
pull repo locks = forEachRepo locks repo $ \x -> do
    (code,out,err) <- cmdCodeOutErr $ "darcs pull --all --summary --repo=" ++ x
    return $ case code of
        ExitSuccess ->
            let n = length [() | x:xs <- lines out, not $ isSpace x] - 3
            in if n <= 0 then Nothing else Just $
                "Pulled " ++ show n ++ " patch" ++ (if n == 1 then "" else "es")
        _ -> Just "Failed"



{-
    sync :: FilePath -> IO ()
    sync dir = do
        b <- doesDirectoryExist $ dir </> "_darcs"
        if b then check dir else do
            xs <- getDirectoryContents dir
            xs <- return [dir </> x | x <- xs, not $ all (== '.') x]
            flip mapM_ xs $ \x -> do
                b <- doesDirectoryExist x
                when b $ sync x
    
    
    check :: FilePath -> IO ()
    check dir = do
        b1 <- run "darcs whatsnew --summary" "No changes!"
        b2 <- run "darcs send --dry-run --all" "No recorded local changes to send!"
        let msg = ["changes"|b1] ++ ["patches"|b2]
        when (msg /= []) $
            putStrLn $ unwords $ [takeFileName dir,"has"] ++ intersperse "and" msg
        where
            run cmd want = do
                let out = dir </> "paper_sync.tmp"
                system $ cmd ++ " --repodir=\"" ++ dir ++ "\" > \"" ++ out ++ "\""
                src <- readFile' out
                return $ want `notElem` lines src
-}



push = undefined
send = undefined
apply = undefined
