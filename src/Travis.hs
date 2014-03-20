{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Travis(run) where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import Text.JSON
import Arguments
import Util


run :: Arguments -> Maybe (IO ())
run Travis = Just $ do
    dir <- getCurrentDirectory
    let name = takeFileName dir
    createDirectoryIfMissing True "dist"
    appendFile "dist/travis" ""
    rel <- findRelevant
    found <- findEntries

    let more [] = more . list =<< wgetJSON ("https://api.travis-ci.org/repos/ndmitchell/" ++ name ++ "/builds")
        more xs = let i = read (last xs ! "number") in if i == 1 then return xs else do
                        new <- wgetJSON $ "https://api.travis-ci.org/repos/ndmitchell/" ++ name ++ "/builds?after_number=" ++ show i
                        more $ xs ++ list new
    builds <- more []
    
    forM_ (reverse builds) $ \x -> do
        let num = x ! "number"
        let id = show (x ! "id" :: Int)
        let time = x ! "started_at"
        when (num `notElem` found && x ! "result" /= JSNull) $ do
            build <- wgetJSON $ "https://api.travis-ci.org/builds/" ++ id
            sleep 2

            let jobs = map (\x -> show (x ! "id" :: Int)) $ build ! "matrix"
            forM_ jobs $ \i -> do
                putChar '.'
                src <- wget $ "https://s3.amazonaws.com/archive.travis-ci.org/jobs/" ++ i ++ "/log.txt"
                let want = [x | x <- lines $ reps '\r' '\n' src, any (`isPrefixOf` x) rel]
                addEntry num time want
            when (null jobs) $ addEntry num time []
    putStrLn "\nDone!"
run _ = Nothing


findRelevant :: IO [String]
findRelevant = do
    src <- fmap lines $ readFile ".travis.yml"
    src <- return $ takeWhile (isPrefixOf " ") $ drop 1 $ dropWhile (not . isPrefixOf "relevant") src
    when (null src) $ error "No relevant lines in the travis file"
    return $ map (dropWhile isSpace) src


findEntries :: IO [String]
findEntries = do
    x <- readFile "dist/travis"
    return [takeWhile isDigit x | x <- lines x, Just x <- ["BUILD " `stripPrefix` x]]


addEntry :: String -> String -> [String] -> IO ()
addEntry num time xs = appendFile "dist/travis" $ unlines $ ("BUILD " ++ num ++ " " ++ time) : xs


wget :: String -> IO String
wget x = withTempFile $ \t -> do
    putStr $ "wget " ++ x ++ " ... "
    cmd $ "wget " ++ x ++ " -O" ++ t ++ " --no-check-certificate --quiet"
    res <- readFile' t
    putStrLn "done"
    return res


wgetJSON :: String -> IO JSValue
wgetJSON x = fmap (ok . decode) $ wget x

ok (Ok x) = x
ok (Error x) = error x

(!) :: JSON a => JSValue -> String -> a
(!) (JSObject mp) k = ok $ valFromObj k mp

list :: JSValue -> [JSValue]
list (JSArray x) = x

has :: String -> JSValue -> Bool
has k (JSObject mp) = k `elem` map fst (fromJSObject mp)
