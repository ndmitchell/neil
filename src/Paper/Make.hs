
module Paper.Make(make) where

import Control.Exception
import Control.Monad
import System.Process
import System.Directory
import System.Exit
import System.FilePath

import Paper.LatexError
import Paper.Util.IO


make :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
make dataDir objDir srcDir mainFile allFiles = do
    let dataFile x = liftM ((dataDir </> x) :)
        sys = System (return ()) (const $ return ()) objDir

    eps1 <- files (srcDir </> "graphics") "eps"
    eps2 <- files srcDir "eps"
    fmt <- dataFile "paper.fmt" $ files srcDir "fmt"
    cls <- files srcDir "cls"
    bib <- dataFile "paper.bib" $ files srcDir "bib"
    tex <- dataFile "paper.tex" $ return allFiles

    for (eps2 ++ fmt ++ cls) $
        \e -> replaceDirectory e objDir <== [e] $ copyFile
    when (not $ null eps1) $ createDirectoryIfMissing True (objDir </> "graphics")
    for eps1 $
        \e -> replaceDirectory e (objDir </> "graphics") <== [e] $ copyFile
    for bib $
        \b -> replaceDirectory b objDir <== [b] $ \from to -> do
            copyFile from to
            system_ sys $ "bibtex -quiet " ++ takeBaseName b
    for tex $
        \t -> replaceDirectory t objDir <== (t:fmt) $ \from to -> do
            -- intermediate copy step because lhs2tex has bugs
            -- which means the input can't be an absolute path
            let temp = to <.> "lhs"
            copyFile from temp
            system_ sys{cleanup=removeFile to} $
                "lhs2tex " ++ takeFileName temp ++ " -o " ++ to
            fixLineEndings to

    let base = takeBaseName mainFile
    system_ sys{errorMsg=latexError (tex++fmt) (objDir </> base <.> "log")} $
        "texify --quiet " ++ (base <.> "tex")
    copyFile (objDir </> base <.> "dvi") (srcDir </> base <.> "dvi")


fixLineEndings file = do
    src <- readFile' file
    writeFile file $ filter (/= '\r') src

for x = flip mapM x

files dir ext = do
    b <- doesDirectoryExist dir
    s <- if b then getDirectoryContents dir else return []
    s <- return $ filter ((==) ('.':ext) . takeExtension) s
    return $ map (dir </>) s



data System = System
    {cleanup :: IO ()
    ,errorMsg :: String -> IO ()
    ,curDir :: FilePath}

system_ sys cmd = do
    orig <- getCurrentDirectory
    bracket_ (setCurrentDirectory (curDir sys)) (setCurrentDirectory orig) $ do
        putStrLn cmd
        res <- system $ cmd ++ " > stdout.txt 2> stderr.txt"
        out <- readFile "stdout.txt"
        err <- readFile "stderr.txt"
        putStr $ out ++ err
        when (res /= ExitSuccess) $ do
            cleanup sys
            errorMsg sys out
            putStrLn "System command failed! Press enter to continue"
            getChar
            exitWith (ExitFailure 1)


(<==) :: FilePath -> [FilePath] -> (FilePath -> FilePath -> IO ()) -> IO ()
(<==) to froms@(from:_) action = do
    b <- doesFileExist to
    rebuild <- if not b then return True else do
        from2 <- liftM maximum $ mapM getModificationTime froms
        to2 <- getModificationTime to
        return $ to2 < from2
    when rebuild $ do
        putStrLn $ "Building: " ++ to
        action from to
