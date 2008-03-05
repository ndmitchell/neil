
module Paper.Make(make) where

import Control.Exception
import Control.Monad
import Data.Char
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath

import Paper.FileData


make :: FilePath -> FileData -> IO ()
make obj src = do
    let dir = directory src
    eps1 <- files (dir </> "graphics") "eps"
    eps2 <- files dir "eps"
    fmt <- files dir "fmt"
    cls <- files dir "cls"
    bib <- files dir "bib"
    tex <- return $ map (dir </>) (allFiles src)

    for (eps1 ++ eps2 ++ fmt ++ cls) $
        \e -> replaceDirectory e obj <== [e] $ copyFile
    for bib $
        \b -> replaceDirectory b obj <== [b] $ \from to -> do
            copyFile from to
            system_ obj $ "bibtex -quiet " ++ takeBaseName b
    for tex $
        \t -> replaceDirectory t obj <== (t:fmt) $ \from to -> do
            -- intermediate copy step because lhs2tex has bugs
            -- which means the input can't be an absolute path
            let temp = to <.> "lhs"
            copyFile from temp
            systemThen_ (removeFile to) obj $
                "lhs2tex " ++ takeFileName temp ++ " -o " ++ to

    system_ obj $ "texify --quiet " ++ mainFile src
    let dvi = replaceExtension (mainFile src) "dvi"
    copyFile (obj </> dvi) (dir </> dvi)


for x = flip mapM x

files dir ext = do
    s <- getDirectoryContents dir
    s <- return $ filter ((==) ('.':ext) . takeExtension) s
    return $ map (dir </>) s



system_ = systemThen_ (return ())

systemThen_ cleanup dir cmd = do
    orig <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory orig) $ do
        putStrLn cmd
        res <- system $ cmd ++ " > stdout.txt 2> stderr.txt"
        out <- readFile "stdout.txt"
        err <- readFile "stderr.txt"
        putStr $ out ++ err
        when (res /= ExitSuccess) $ do
            cleanup
            reportError out
            putStrLn "System command failed! Press enter to continue"
            getChar
            exitWith (ExitFailure 1)


reportError :: String -> IO ()
reportError s2 = do
        b <- doesFileExist file
        when (b && not (null pos) && all isDigit pos) $ do
            src <- readFile file
            putStr $ unlines $ map f $ take 7 $ drop (read pos - 3) $ zip [1..] $ lines src
    where
        s = last $ "" : lines s2
        (pre,post) = splitAt 3 s
        (front,rest) = break (== ':') post
        file = pre ++ front
        pos = takeWhile (/= ':') $ drop 1 rest
        f (p,s) = show p ++ " : " ++ s


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
