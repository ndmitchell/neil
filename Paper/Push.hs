
module Paper.Push(push) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Directory
import System.FilePath


push :: FilePath -> IO ()
push x = do
    r <- repo $ reverse $ map joinPath $ tail $ inits $ splitDirectories x
    when (isNothing r) $ error $ "Repo not found above: " ++ x

    src <- readFile $ fromJust r </> "_darcs" </> "prefs" </> "repos"
    () <- length src `seq` return ()
    let ans = filter (not . ("http://" `isPrefixOf`)) $ lines src
    when (null ans) $ error "No non-http repos in the prefs/repos file"
    system $ "darcs push --no-set-default \"" ++ (head ans) ++ "\""
    return ()


repo :: [FilePath] -> IO (Maybe FilePath)
repo [] = return Nothing
repo (x:xs) = do
    b <- doesDirectoryExist (x </> "_darcs")
    if b then return $ Just x else repo xs
