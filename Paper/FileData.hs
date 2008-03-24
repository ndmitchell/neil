
module Paper.FileData(FileData(..), allFiles, getFileData) where

import Control.Monad
import Data.List
import Data.Ord
import System.Directory
import System.FilePath


-- | Each mainFile/extraFiles must be a pure file name, no directory.
--   All have directory appended to them first.
data FileData = FileData
    {directory :: String
    ,mainFile :: String
    ,extraFiles :: [String]
    ,flags :: [String]
    }
    deriving Show


allFiles :: FileData -> [String]
allFiles x = mainFile x : extraFiles x



getFileData :: [String] -> IO FileData
getFileData [] = do
    s <- getCurrentDirectory
    getFileData [s]

getFileData (('-':flag):ss) = do
    rest <- getFileData ss
    return rest{flags = flag : flags rest}

getFileData (s:ss) = do
    b <- doesDirectoryExist s
    if b then
        if null ss then getDirData s
        else error "If you specify a directory you may give only one argument"
     else getFilesData (s:ss)


getFilesData files = do
    files <- mapM canonicalizePath files
    found <- mapM doesFileExist files
    case filter snd $ zip files found of
        x:_ -> error $ "File not found, " ++ fst x
        [] -> do
            let (dirs,files2) = unzip $ map splitFileName files
                dirs2 = nub dirs
            when (length dirs2 > 1) $ error $ "Files must all be in the same directory: " ++ show dirs2
            return $ FileData (dropTrailingPathSeparator $ head dirs) (head files2) (tail files2) []


getDirData dir = do
    dir <- canonicalizePath dir
    files <- getDirectoryContents dir
    files <- return $ filter ((==) ".tex" . takeExtension) files
    when (null files) $ error $ "No files found in directory, " ++ dir

    -- now pick the main file
    let mainFile = if "index.tex" `elem` files then "index.tex" else
                   snd $ minimumBy (comparing fst) [(rank x, x) | x <- files]
        dirs = reverse $ splitDirectories dir
        rank x = liftM negate $ findIndex (== dropExtension x) dirs

    return $ FileData dir mainFile (delete mainFile files) []
