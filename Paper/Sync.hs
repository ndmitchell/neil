
module Paper.Sync(sync) where

import Control.Monad
import Data.List
import System.Cmd
import System.Directory
import System.FilePath
import Paper.Util.IO


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
