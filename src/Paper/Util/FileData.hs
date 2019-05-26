
module Paper.Util.FileData(
    FileData(..), getFileData
    ) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath


data FileData = FileData
    {directory :: FilePath       -- ^ All files must reside in one directory
    ,mainFile  :: FilePath       -- ^ The main file
    ,argFiles  :: [FilePath]     -- ^ Files given on the command line
    ,allFiles  :: [FilePath]     -- ^ All files in the directory
    ,flags     :: [String]       -- ^ Any flags given
    ,darcs     :: FilePath       -- ^ The location of the _darcs directory
    }
    deriving Show



getFileData :: [String] -> IO FileData
getFileData args = do
    let (opt,files) = partition ("-" `isPrefixOf`) args
    files <- if null files then liftM (:[]) getCurrentDirectory else return files
    (dirs,explicit,implicit) <- liftM unzip3 $ mapM f files
    (explicit,implicit) <- return (concat explicit, concat implicit)

    when (length (nub dirs) > 1) $
        error "All files must be from the same directory"

    let snub = nub . sort
        nullErr x | null explicit = error $ "Error: No Latex files found in " ++ show (head dirs)
                  | otherwise = x

    let dir = head dirs
    darcs <- getDarcs dir

    return $ FileData
        dir
        (nullErr $ head $ explicit)
        (nullErr $ snub $ explicit)
        (nullErr $ snub $ explicit ++ implicit)
        (map tail opt)
        darcs
    where
        -- return (directory, explicit, implicit)
        f file = do
            file <- canonicalizePath file
            bDir <- doesDirectoryExist file
            bFile <- doesFileExist file
            if bDir then getDir file
             else if bFile then getFile file
             else error $ "Error: Could not find file " ++ show file



getDir dir = do
    files <- getDirectoryContents dir
    files <- return $ filter ((==) ".tex" . takeExtension) files

    -- now pick the main file
    let mainFile = snd $ maximum [(rank x, x) | x <- files]
        dirs = reverse $ splitDirectories dir
        rank x = liftM negate $ elemIndex (dropExtension x) dirs
    return (dir, [mainFile | not $ null files] ++ files, [])


getFile file = do
    (a,b,[]) <- getDir $ takeDirectory file
    return (a, [file], b)


getDarcs = f . reverse . map joinPath . tail . inits . splitDirectories
    where
        f [] = return $ error "Darcs repo not found"
        f (x:xs) = do
            b <- doesDirectoryExist (x </> "_darcs")
            if b then return $ dropTrailingPathSeparator x else f xs
