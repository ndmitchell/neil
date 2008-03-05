
module Paper.Make(make) where

import Control.Exception
import Control.Monad
import Data.Char
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath

import Paper.FileData
import Paper.LatexError


make :: FilePath -> FilePath -> FileData -> IO ()
make dat obj src = do
    let dir = directory src
        dat_ x = liftM ((dat </> x) :)
        sys = System (return ()) (const $ return ()) obj

    eps1 <- files (dir </> "graphics") "eps"
    eps2 <- files dir "eps"
    fmt <- dat_ "paper.fmt" $ files dir "fmt"
    cls <- files dir "cls"
    bib <- dat_ "paper.bib" $ files dir "bib"
    tex <- dat_ "paper.tex" $ return $ map (dir </>) (allFiles src)

    for (eps1 ++ eps2 ++ fmt ++ cls) $
        \e -> replaceDirectory e obj <== [e] $ copyFile
    for bib $
        \b -> replaceDirectory b obj <== [b] $ \from to -> do
            copyFile from to
            system_ sys $ "bibtex -quiet " ++ takeBaseName b
    for tex $
        \t -> replaceDirectory t obj <== (t:fmt) $ \from to -> do
            -- intermediate copy step because lhs2tex has bugs
            -- which means the input can't be an absolute path
            let temp = to <.> "lhs"
            copyFile from temp
            system_ sys{cleanup=removeFile to} $
                "lhs2tex " ++ takeFileName temp ++ " -o " ++ to

    let log = obj </> replaceExtension (mainFile src) "log"
    system_ sys{errorMsg=latexError tex log} $
        "texify --quiet " ++ mainFile src
    let dvi = replaceExtension (mainFile src) "dvi"
    copyFile (obj </> dvi) (dir </> dvi)


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
