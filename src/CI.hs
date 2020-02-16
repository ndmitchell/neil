{-# LANGUAGE RecordWildCards #-}

module CI(run) where

import Control.Monad
import Data.Char
import Data.List.Extra hiding (list)
import Data.Maybe
import System.Directory
import System.IO.Extra
import System.FilePath
import System.Time.Extra
import System.Process.Extra
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import Arguments


run :: Arguments -> Maybe (IO ())
run Travis{..} = Just $ do
    dir <- getCurrentDirectory
    let name = takeFileName dir
    createDirectoryIfMissing True "dist"
    appendFile "dist/travis" ""
    rel <- findRelevant
    found <- findEntries

    let more [] = more . list =<< wgetJSON wait ("https://api.travis-ci.org/repos/ndmitchell/" ++ name ++ "/builds")
        more xs = let i = read (last xs ! "number") in if i == 1 then return xs else do
                        new <- wgetJSON wait $ "https://api.travis-ci.org/repos/ndmitchell/" ++ name ++ "/builds?after_number=" ++ show i
                        more $ xs ++ list new
    builds <- more []

    forM_ (reverse builds) $ \x -> do
        let num = x ! "number"
        let id = show (x ! "id" :: Int)
        let time = x ! "started_at"
        when (num `notElem` found && x ! "result" /= Null) $ do
            build <- wgetJSON wait $ "https://api.travis-ci.org/builds/" ++ id
            sleep 2

            let jobs = map (\x -> show (x ! "id" :: Int)) $ build ! "matrix"
            forM_ jobs $ \i -> do
                putChar '.'
                src <- wget wait $ "https://s3.amazonaws.com/archive.travis-ci.org/jobs/" ++ i ++ "/log.txt"
                let want = [x | x <- lines $ replace "\r" "\n" src, any (`isPrefixOf` x) rel]
                addEntry num time want
            when (null jobs) $ addEntry num time []
    putStrLn "\nDone!"
run _ = Nothing


findRelevant :: IO [String]
findRelevant = do
    src <- fmap lines $ readFile ".travis.yml"
    src <- return $ takeWhile (isPrefixOf " ") $ drop 1 $ dropWhile (not . isPrefixOf "relevant") src
    when (null src) $ error "No relevant lines in the travis file"
    return $ map trimStart src


findEntries :: IO [String]
findEntries = do
    x <- readFile "dist/travis"
    return [takeWhile isDigit x | x <- lines x, Just x <- ["BUILD " `stripPrefix` x]]


addEntry :: String -> String -> [String] -> IO ()
addEntry num time xs = appendFile "dist/travis" $ unlines $ ("BUILD " ++ num ++ " " ++ time) : xs


wget :: Double -> String -> IO String
wget wait x = withTempFile $ \t -> do
    putStr $ "wget " ++ x ++ " ... "
    system_ $ "wget " ++ x ++ " -O" ++ t ++ " --no-check-certificate --quiet"
    res <- readFile' t
    putStrLn "done"
    sleep wait
    return res


wgetJSON :: Double -> String -> IO Value
wgetJSON wait x = fmap (fromJust . decode . LBS.pack) $ wget wait x

(!) :: FromJSON a => Value -> String -> a
(!) (Object mp) k = fromSuccess $ fromJSON $ mp Map.! T.pack k

fromSuccess (Error x) = error x
fromSuccess (Success x) = x

list :: Value -> [Value]
list (Array x) = V.toList x
